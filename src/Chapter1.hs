{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Chapter1
  ( IPAddress(..)
  , LookupResult(..)
  , fetchGeoIP
  ) where

import           ClassyPrelude
import           Data.Aeson
import           Network.HTTP.Simple

-- | Represents an IP address like 172.217.22.46
newtype IPAddress = IPAddress
  { getAddress :: String
  } deriving (Show, Generic, FromJSON)

-- | Allows us to write ("172.217.22.46" :: IPAddress)
instance IsString IPAddress where
  fromString = IPAddress

-- | Result of a GeoIP lookup. Record names match payload for convenient
--   JSON decoding.
data LookupResult = LookupResult
  { country_code :: Text
  , country_name :: Text
  } deriving (Show, Generic, FromJSON)

-- | Query the server and get a result. Throws an exception if anything fails.
fetchGeoIP :: IPAddress -> IO LookupResult
fetchGeoIP ipAddr = do
  req <- parseRequest ("http://freegeoip.net/json/" <> getAddress ipAddr)
  getResponseBody <$> httpJSON req

main :: IO ()
main = fetchGeoIP "172.217.22.46" >>= print
