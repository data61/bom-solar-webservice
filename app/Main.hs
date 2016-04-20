{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Main where

import Lib

import Control.Applicative

import Data.Text (unpack, pack)
import Foreign.C.Types (CShort, CDouble, CInt)

import Servant.API
import Servant
import Servant.CSV.Cassava
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)

import Servant.Swagger
import Data.Swagger
import Data.Swagger.ParamSchema
import Data.Swagger.Lens -- (type_)
import Control.Lens hiding ((.=))
import Data.HashMap.Strict ()

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.IO.Class

import Data.Aeson hiding ((.=))
import Data.Csv
import Data.Maybe (fromJust, fromMaybe)

import Data.Time.Clock
import Data.Time.Format
import Data.Time.Calendar

import Data.NetCDF
import Data.NetCDF.Vector ()
import qualified Data.Vector.Storable as SV
import           Data.Vector.Storable (Vector)

import Data.Coerce
import GHC.Generics

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.MVar

nlats,nlons,nhours :: Int
nlats = 679
nlons = 839
nhours = 227928

firstHour :: UTCTime
firstHour = UTCTime (fromGregorian 1989 12 31) (secondsToDiffTime 0)

hour :: NominalDiffTime
hour = 60*60 -- Seconds

type AppAPI
  = "swagger.json" :> Get '[JSON] Swagger
    :<|> BoMAPI

appProxy :: Proxy AppAPI
appProxy = Proxy

type BoMAPI
  = "v1" :> BomAPIv1

bomProxy :: Proxy BoMAPI
bomProxy = Proxy

type BomAPIv1
  = "DNI"
      :> Capture "lat" Double :> Capture "lon" Double
      :> QueryParam "start" ISO8601 :> QueryParam "end" ISO8601
      :> Get '[(CSV',DefaultEncodeOpts)] [DNIVal]

iso8601Format = iso8601DateFormat $ Just "%H:%M:%S%z"
newtype ISO8601 = ISO8601 UTCTime
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance ToField ISO8601 where
  toField (ISO8601 utc) = toField $ formatTime defaultTimeLocale iso8601Format utc
instance FromText ISO8601 where
  fromText t = ISO8601 <$> parseTimeM False defaultTimeLocale iso8601Format (unpack t)
instance ToParamSchema ISO8601 where
  toParamSchema _ = mempty
    & schemaType .~ SwaggerString
    & schemaFormat .~ Just (pack iso8601Format)


newtype PosInt = PosInt Int
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
instance ToField PosInt where
  toField (PosInt i) = if i < 0
    then toField ("-" :: String)
    else toField i

data DNIVal = DNIVal
  {dniTime :: {-# UNPACK #-}!ISO8601
  , dniVal :: {-# UNPACK #-}!PosInt
  }
  deriving (Eq,Show,Generic)

instance ToJSON DNIVal
instance FromJSON DNIVal
instance DefaultOrdered DNIVal where
  headerOrder _ = ["UTC time","DNI"]
instance ToNamedRecord DNIVal where
  toNamedRecord (DNIVal time val) = namedRecord
    [ "UTC time" .= time
    , "DNI"  .= val
    ]
instance ToSchema PosInt
instance ToSchema ISO8601
instance ToSchema DNIVal
-- where
--   declareNamedSchema _ = pure (Just "DNIVal", schema) where
--     schema = mempty
--       & schemaType .~ SwaggerObject
--       & schemaProperties .~
--           [("dniTime", toSchemaRef (Proxy :: Proxy ISO8601))
--           ,("dniVal" , toSchemaRef (Proxy :: Proxy PosInt))
--           ]
--       & schemaRequired .~ ["dniTime", "dniVal"]

retriveTimeSeries
  :: Chan (Double,Double, Maybe ISO8601, Maybe ISO8601, MVar (Either String (Vector CInt)))
  -> Double -> Double --
  -> Maybe ISO8601 -> Maybe ISO8601
  -> EitherT ServantErr IO [DNIVal]
retriveTimeSeries ch lat lon mstart mend = do
  evec <- liftIO $ do
    ret <- newEmptyMVar
    writeChan ch (lat,lon,mstart,mend,ret)
    race (threadDelay 200000 >> return "Timed out") (takeMVar ret)
  case join evec of
    Left err -> left err500{errReasonPhrase = errReasonPhrase err500 ++ " ("++err++")"}
    Right vec
      -> pure $ zipWith (coerce DNIVal)
                    (iterate (addUTCTime hour) (fromMaybe firstHour (coerce mstart)))
                    (map (fromIntegral :: CInt -> Int) $ SV.toList vec)


main :: IO ()
main = do
  enc <- liftIO $ openFile "all-DNI-reformat.nc"
  case enc of
    Left err -> fail $ show err
    Right nc -> do
      case (,,) <$> ncVar nc "Band1" <*> ncVar nc "lat" <*> ncVar nc  "lon" of
        Nothing -> fail "Band1 variable not found in NetCDF file"
        Just (band1,latsv,lonsv) -> do
          Right lats <- get nc latsv
          Right lons <- get nc lonsv
          ch <- newChan

          -- print =<< (runEitherT $ retriveTimeSeries nc band1 (cdToD lats) (cdToD lons)  (-27) 137 Nothing Nothing)
          forkIO $ run 8080 $ simpleCors $ serve appProxy (pure (toSwagger bomProxy) :<|> retriveTimeSeries ch)
          vectorServer nc band1 (cdToD lats) (cdToD lons) ch

  where cdToD :: Vector CDouble -> Vector Double
        cdToD = coerce


vectorServer :: NcInfo NcRead -> NcVar -> Vector Double -> Vector Double
  -> Chan (Double,Double,Maybe ISO8601, Maybe ISO8601,MVar (Either String (Vector CInt)))
  -> IO ()
vectorServer nc band1 lats lons ch = forever $ do
  (lat,lon,mstart,mend,ret) <- readChan ch
  let lati = vectorIndex LT FromStart lats lat
      loni = vectorIndex LT FromStart lons lon
      sidx = maybe 0      (max 0)      (getOffset mstart)
      eidx = maybe nhours (min nhours) (getOffset mend  )
  if eidx < sidx
    then putMVar ret (Left $ unwords ["End time before start time, start:",show mstart,"end:",show mend])
    else do
      evec <- getA nc band1 [lati,loni,sidx] [1,1,eidx-sidx]
      putMVar ret $ case evec of
        Left err -> Left (show err)
        Right vec -> Right vec
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
