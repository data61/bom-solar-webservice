{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Main where

import Lib


-- import Data.Text hiding (zip)
import Foreign.C.Types (CShort, CDouble, CInt)

import Servant.API
import Servant
import Servant.CSV.Cassava
import Network.Wai.Handler.Warp (run)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.IO.Class

import Data.Aeson hiding ((.=))
import Data.Csv

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

type BomAPI
  = "v1" :> BomAPIv1

bomProxy :: Proxy BomAPI
bomProxy = Proxy

type BomAPIv1
  = "DNI"
      :> Capture "lat" Double :> Capture "lat" Double
      -- :> QueryParam "start" UTCTime :> QueryParam "end" UTCTime
      :> Get '[(CSV',DefaultEncodeOpts),JSON] [TimeVal]

newtype ISO8601 = ISO8601 UTCTime
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance ToField ISO8601 where
  toField (ISO8601 utc) = toField $ formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S%z") utc

newtype PosInt = PosInt Int
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
instance ToField PosInt where
  toField (PosInt i) = if i < 0
    then toField ("-" :: String)
    else toField i

data TimeVal = TimeVal
  {tvTime :: {-# UNPACK #-}!ISO8601
  , tvVal :: {-# UNPACK #-}!PosInt
  }
  deriving (Eq,Show,Generic)

instance ToJSON TimeVal
instance FromJSON TimeVal
instance DefaultOrdered TimeVal where
  headerOrder _ = ["UTC time","DNI"]
instance ToNamedRecord TimeVal where
  toNamedRecord (TimeVal time val) = namedRecord
    [ "UTC time" .= time
    , "DNI"  .= val
    ]


retriveTimeSeries
  :: Chan (Double,Double, MVar (Either String (Vector CInt)))
  -> Double -> Double --
  -- -> Maybe UTCTime -> Maybe UTCTime
  -> EitherT ServantErr IO [TimeVal]
retriveTimeSeries ch lat lon = do -- _mstart _mend = do
  evec <- liftIO $ do
    ret <- newEmptyMVar
    writeChan ch (lat,lon,ret)
    race (threadDelay 200000 >> return "Timed out") (takeMVar ret)
  case join evec of
    Left err -> left err500{errReasonPhrase = errReasonPhrase err500 ++ " ("++err++")"}
    Right vec
      -> pure $ zipWith (coerce TimeVal)
                    (iterate (addUTCTime hour) firstHour)
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
          forkIO $ run 8080 $ serve bomProxy (retriveTimeSeries ch)
          vectorServer nc band1 (cdToD lats) (cdToD lons) ch


vectorServer :: NcInfo NcRead -> NcVar -> Vector Double -> Vector Double
  -> Chan (Double,Double,MVar (Either String (Vector CInt)))
  -> IO ()
vectorServer nc band1 lats lons ch = forever $ do
  (lat,lon,ret) <- readChan ch
  let lati = vectorIndex LT FromStart lats lat
      loni = vectorIndex LT FromStart lons lon
  evec <- getA nc band1 [lati,loni,0] [1,1,nhours]
  putMVar ret $ case evec of
    Left err -> Left (show err)
    Right vec -> Right vec


cdToD :: Vector CDouble -> Vector Double
cdToD = coerce


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
