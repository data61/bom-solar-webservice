{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}


module Main where

import           Control.Applicative

import           Data.Text                            (pack, unpack)
import           Foreign.C.Types                      (CDouble, CInt, CShort)

import           Servant                              as S
import           Servant.API
import           Servant.CSV.Cassava
import           Web.HttpApiData

import           Data.Default
import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.Cors          (simpleCors)
import           Network.Wai.Middleware.Gzip          (gzip)
import           Network.Wai.Middleware.RequestLogger (Destination (..),
                                                       IPAddrSource (..),
                                                       OutputFormat (..),
                                                       RequestLoggerSettings (..),
                                                       mkRequestLogger)

import           System.IO                            (BufferMode (..),
                                                       IOMode (..),
                                                       hSetBuffering, openFile)

import           Control.Lens                         hiding ((.=))
import           Data.HashMap.Strict                  ()
import           Data.Swagger
import           Data.Swagger.Lens                    as S
import           Data.Swagger.ParamSchema
import           Servant.Swagger

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Either

import           Data.Aeson                           hiding ((.=))
import qualified Data.Aeson                           as A
import           Data.Csv                             as Csv
import           Data.Maybe                           (fromJust, fromMaybe)

import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format

import           Data.NetCDF                          (NcInfo, NcRead, NcVar,
                                                       ncVar)
import qualified Data.NetCDF                          as NC
import           Data.NetCDF.Vector                   ()
import qualified Data.Vector                          as V
import qualified Data.Vector.Generic                  as G
import           Data.Vector.Storable                 (Vector)
import qualified Data.Vector.Storable                 as SV

import           Data.Coerce
import           GHC.Generics

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar

import           Configuration.Utils
import           PkgInfo_bom_solar_webservice

import           System.IO                            (BufferMode (NoBuffering),
                                                       hSetBuffering, stdout)
import           Text.Heredoc                         (there)
import           Text.Printf                          (printf)

--
-- Configuration types
--
data BoMSolarConf = BoMSolarConf
  {_bsHttpPort  :: Int
  ,_bsAccessLog :: FilePath
  ,_bsNetCDF    :: FilePath
  }
$(makeLenses ''BoMSolarConf)

defaultBoMSolarConf :: BoMSolarConf
defaultBoMSolarConf = BoMSolarConf
  {_bsHttpPort = 3003
  ,_bsAccessLog = "access.log"
  ,_bsNetCDF = ""
  }

instance FromJSON (BoMSolarConf -> BoMSolarConf) where
  parseJSON = withObject "BoMSolarConf" $ \o -> id
    <$< bsHttpPort  ..: "bsHttpPort"  % o
    <*< bsAccessLog ..: "bsAccessLog" % o
    <*< bsNetCDF    ..: "bsNetCDF" % o

instance ToJSON BoMSolarConf where
  toJSON a = object
    ["bsHttpPort"  A..= _bsHttpPort a
    ,"bsAccessLog" A..= _bsAccessLog a
    ,"bsNetCDF"    A..= _bsNetCDF a
    ]

pBoMSolarConf :: MParser BoMSolarConf
pBoMSolarConf = id
  <$< bsHttpPort  .:: option auto % short 'P' <> long "http-port"  <> metavar "PORT" <> help "HTTP Port for webservice"
  <*< bsAccessLog .:: strOption   % short 'a' <> long "access-log" <> metavar "HTTPACCESS" <> help "HTTP Access log file"
  <*< bsNetCDF    .:: strOption   % short 'n' <> long "netcdf-file" <> metavar "FILE.nc" <> help "NetCDF file matching format in README"


--
-- HTTP Routes
--

type AppAPI
  = "solar-satellite" :>
      ("swagger.json" :> S.Header "Host" String :> Get '[JSON] Swagger
      :<|> BoMAPI
      )

appProxy :: Proxy AppAPI
appProxy = Proxy

type BoMAPI
  = "v1" :> BomAPIv1

bomProxy :: Proxy ("solar-satellite" :> BoMAPI)
bomProxy = Proxy

type BomAPIv1
  = "DNI"
      :> Capture "lat" Double :> Capture "lon" Double
      :> QueryParam "start" ISO8601 :> QueryParam "end" ISO8601
      :> Get '[(CSV',DefaultEncodeOpts), JSON] (Headers '[S.Header "Content-Disposition" String] (V.Vector TimedVal))


main :: IO ()
main = runWithPkgInfoConfiguration mainInfo pkgInfo $ \config -> do
  hSetBuffering stdout NoBuffering
  enc <- liftIO $ NC.openFile (config ^. bsNetCDF)
  case enc of
    Left err -> fail $ show err
    Right nc -> do
      case (,,) <$> ncVar nc "Band1" <*> ncVar nc "lat" <*> ncVar nc  "lon" of
        Nothing -> fail $ "variable not found in NetCDF file" ++ config ^. bsNetCDF
        Just (band1,latsv,lonsv) -> do
          Right lats <- NC.get nc latsv
          Right lons <- NC.get nc lonsv
          ch <- newChan
          middleware <- makeMiddleware config
          -- print =<< (runEitherT $ retriveTimeSeries nc band1 (cdToD lats) (cdToD lons)  (-27) 137 Nothing Nothing)
          forkIO
            . run (config ^. bsHttpPort)
            . middleware
            $ serve appProxy ((pure . appSwagger) :<|> retriveTimeSeries ch)
          vectorServer nc band1 (cdToD lats) (cdToD lons) ch

  where cdToD :: Vector CDouble -> Vector Double
        cdToD = coerce

mainInfo :: ProgramInfo BoMSolarConf
mainInfo = programInfo "BoM Solar Webservice" pBoMSolarConf defaultBoMSolarConf

appSwagger :: Maybe String -> Swagger
appSwagger mhost = toSwagger bomProxy
  & S.info.title .~ "BoM Solar"
  & S.info.version .~ versionString
  -- & S.info.infoContact ?~ Contact Nothing Nothing Nothing
  & S.info.contact.non (Contact Nothing Nothing Nothing).S.name ?~ "Alex Mason"
  & S.info.contact._Just.email ?~ "aremi@nicta.com.au"
  & S.info.description ?~ [there|Description.md|]
  & S.info.S.license ?~ License "Apache 2.0" (Just $ URL "http://www.apache.org/licenses/LICENSE-2.0")
  & host ?~ Host (maybe "services.aremi.nicta.com.au" id mhost) Nothing


makeMiddleware :: BoMSolarConf -> IO Middleware
makeMiddleware conf = do
  h <- openFile (conf ^. bsAccessLog) AppendMode
  hSetBuffering h NoBuffering
  accessLogger <- mkRequestLogger def
      {destination = Handle h
      ,outputFormat = Apache FromFallback
      ,autoFlush = True}
  return $ accessLogger . simpleCors

--
-- Constants
--
nlats,nlons,nhours :: Int
nlats = 679
nlons = 839
nhours = 227928

firstHour :: UTCTime
firstHour = UTCTime (fromGregorian 1989 12 31) (secondsToDiffTime 0)

hour :: NominalDiffTime
hour = 60*60 -- Seconds


-- | A datatype representing a UTCTime shown and read using the ISO 8601 format with HH:MM:SS and timezone
newtype ISO8601 = ISO8601 UTCTime deriving (Eq, Generic, ToJSON, FromJSON)
iso8601Format = iso8601DateFormat $ Just "%H:%M:%S%z"
instance Show            ISO8601 where show (ISO8601 t) = formatTime defaultTimeLocale iso8601Format t
instance Read            ISO8601 where readsPrec p = (coerce :: ReadS UTCTime -> ReadS ISO8601) $ readSTime True defaultTimeLocale iso8601Format
instance ToField         ISO8601 where toField (ISO8601 utc) = toField $ formatTime defaultTimeLocale iso8601Format utc
instance FromHttpApiData ISO8601 where parseQueryParam t = ISO8601 <$> parseTimeM False defaultTimeLocale iso8601Format (unpack t)
instance ToParamSchema   ISO8601 where
  toParamSchema _ = mempty
    & type_ .~ SwaggerString
    & format .~ Just (pack iso8601Format)


newtype PosInt = PosInt Int deriving (Eq, Show, Generic, ToJSON, FromJSON)
instance ToField PosInt where
  toField (PosInt i) = if i < 0
    then toField ("-" :: String)
    else toField i

data TimedVal = TimedVal
  {tvTime :: {-# UNPACK #-}!ISO8601
  , tvVal :: {-# UNPACK #-}!PosInt
  } deriving (Eq,Show,Generic)

instance ToJSON         TimedVal
instance FromJSON       TimedVal
instance DefaultOrdered TimedVal where headerOrder _ = ["UTC time","Value"]
instance ToNamedRecord  TimedVal where
  toNamedRecord (       TimedVal time val) = namedRecord
    [ "UTC time" Csv..= time
    , "Value"    Csv..= val
    ]
instance ToSchema PosInt where
  declareNamedSchema _ = pure (NamedSchema (Just "PosInt") schema) where
    schema = mempty
      & minProperties ?~ 0
      & maxProperties ?~ 1200
      & enum_ ?~ ["-",Number 0, Number 1200]
instance ToSchema ISO8601
instance ToSchema TimedVal
-- where
--   declareNamedSchema _ = pure (Just "TimedVal", schema) where
--     schema = mempty
--       & schemaType .~ SwaggerObject
--       & schemaProperties .~
--           [("dniTime", toSchemaRef (Proxy :: Proxy ISO8601))
--           ,("dniVal" , toSchemaRef (Proxy :: Proxy PosInt))
--           ]
--       & schemaRequired .~ ["dniTime", "dniVal"]


-- | Handler for producing Solar data
retriveTimeSeries :: Chan VecReq -- ^ Channel to send requests for vectors
                  -> Double -- ^ Latitude
                  -> Double -- ^ Longitude
                  -> Maybe ISO8601 -- ^ Start time
                  -> Maybe ISO8601 -- ^ End time
                  -> Handler (Headers '[S.Header "Content-Disposition" String] (V.Vector TimedVal))
retriveTimeSeries ch lat lon mstart mend = do
  evec <- liftIO $ do
    start <- getCurrentTime
    ret <- newEmptyMVar
    writeChan ch (VecReq lat lon mstart mend ret)
    evec <- race (threadDelay (60*1000000) >> return TimedOut) (takeMVar ret)
    end <- getCurrentTime
    printf "%s - %s (%fs) for lat/lon %f %f\n" (fmtTime start) (fmtTime end) (fromRational $ toRational $ diffUTCTime end start :: Double) lat lon
    pure evec
  case join evec of
    Left (StrErr err)           -> throwError err500{errReasonPhrase = errReasonPhrase err500 ++ " ("++err++")"}
    Left (NotFound lat lon)     -> throwError err404{errReasonPhrase = "Lat or Lon out of bounds ("++show lat++","++show lon++")"}
    Left (RangeErr mstart mend) -> throwError err400{errReasonPhrase = "Start time is before end time ("++show mstart++","++show mend++")"}
    Left TimedOut               -> throwError err500{errReasonPhrase = "Request timed out"}
    Right vec ->
      pure $
      -- TODO Change to DNI/DHI if we ever support both
      addHeader (printf "filename=dni_%.5f_%.5f.csv" lat lon) $
      V.zipWith (coerce TimedVal)
              (V.iterateN (SV.length vec) (addUTCTime hour) $ fromMaybe firstHour (coerce mstart))
              -- (SV.map (fromIntegral :: CInt -> Int) $ vec)
              (G.convert $ SV.map (fromIntegral :: CInt -> Int) $ vec)
    where fmtTime = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%Z"))


data VecReq = VecReq
  {vrLat   :: !Double          -- ^ Latitude
  ,vrLon   :: !Double          -- ^ Longitude
  ,vrStart :: !(Maybe ISO8601) -- ^ Start time
  ,vrEnd   :: !(Maybe ISO8601) -- ^ End time
  ,vrRet   :: !(MVar (Either VecReqErr (Vector CInt))) -- ^ Return variable, with Either an error or the vector
  }

data VecReqErr
  = NotFound Double Double
  | RangeErr (Maybe ISO8601) (Maybe ISO8601)
  | StrErr String
  | TimedOut

-- | A server function which responds to reauests for data
-- for specific a latitude and longitude, and optional start and
-- end times.
--
-- Normally this would be handled directly in the handler, but
-- NetCDF is awful and fails to read the NetCDF files unless called from
-- the `main' thread.
vectorServer :: NcInfo NcRead -- ^ NetCDF File info
             -> NcVar         -- ^ Variable from NetCDF File to read
             -> Vector Double -- ^ Vector of latitudes
             -> Vector Double -- ^ Vector of longitudes
             -> Chan VecReq   -- ^ Channel to await requests on
             -> IO ()
vectorServer nc band1 lats lons ch = forever $ do
  (VecReq lat lon mstart mend ret) <- readChan ch
  let lati = vectorIndex LT FromStart lats lat
      loni = vectorIndex LT FromStart lons lon
      sidx = maybe 0      (max 0)      (getOffset mstart)
      eidx = maybe nhours (min nhours) (getOffset mend  )
  if | eidx < sidx -> putMVar ret (Left $ RangeErr mstart mend)
     | lati == -1 || loni == -1 -> putMVar ret (Left $ NotFound lat lon)
     | otherwise -> do
        evec <- NC.getA nc band1 [lati,loni,sidx] [1,1,eidx-sidx]
        putMVar ret $ case evec of
          Left err -> Left (StrErr $ show err)
          Right vec -> if SV.all (== -999) vec then Left (NotFound lat lon) else Right vec
  where
    getOffset :: Maybe ISO8601 -> Maybe Int
    getOffset (Just (ISO8601 utc))
      | dif < 0 = Nothing
      | dif <= fromIntegral nhours * hour
        = case properFraction dif of
          (secs,_) -> case secs `div` (60*60) of
            hours -> Just (fromInteger hours)
      where dif = diffUTCTime utc firstHour
    getOffset _ = Nothing


data IndexStart = FromStart | FromEnd

vectorIndex :: (SV.Storable a, Ord a)
            => Ordering -> IndexStart -> SV.Vector a -> a -> Int
vectorIndex o s v val = case (go o, s) of
  (Nothing, _) -> (-1)
  (Just i, FromStart) -> i
  (Just i, FromEnd) -> SV.length v - 1 - i
  where go LT = SV.findIndex (>= val) vord
        go GT = SV.findIndex (<= val) vord
        go _  = error "Cannot compare to EQ"
        vord = case s of
          FromStart -> v
          FromEnd -> SV.reverse v
