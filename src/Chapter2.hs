{-# LANGUAGE RecordWildCards #-}

module Chapter2
  ( JobResult
  , GeoIPLookup(..)
  , mkGeoIPLookup
  , lookupIP
  ) where

import ClassyPrelude
import System.Console.Concurrent

import Chapter1
import IPs

-- | Because IO actions can throw exceptions we have to deal with that
--   at some point. I decided to not encode failure into LookupResult.
type JobResult = Either SomeException LookupResult

-- | A data type holding the queue and lookup implementation.
data GeoIPLookup = GeoIPLookup
  { ipLookup      :: IPAddress -> IO LookupResult
  , ipLookupQueue :: TQueue (IPAddress, TVar (Maybe JobResult))
  }

-- | Creates a new lookup with n amount of worker threads
mkGeoIPLookup :: Int -> (IPAddress -> IO LookupResult) -> IO GeoIPLookup
mkGeoIPLookup n f = do
  glookup <- GeoIPLookup <$> pure f <*> newTQueueIO
  processQueue glookup n
  return glookup

-- | New api for looking up IPs.
lookupIP :: GeoIPLookup -> IPAddress -> IO (Async JobResult)
lookupIP l ipAddr = async $ do
  var <- newTVarIO Nothing
  atomically $ writeTQueue (ipLookupQueue l) (ipAddr, var)

  -- This is the interesting part: We block the thread until the value
  -- in the TVar is (Just x). STM will do the rest for us!
  -- Alternatively: atomically (readTVar var >>= maybe retrySTM return)
  atomically $ do
    done <- readTVar var
    case done of
      Nothing  -> retrySTM
      Just res -> return res

-- | Just spawns a bunch of worker threads
processQueue :: GeoIPLookup -> Int -> IO ()
processQueue l n = replicateM_ n (worker l)

-- | A worker is just taking the next IP out of the queue and looks it up
worker :: GeoIPLookup -> IO ()
worker GeoIPLookup {..} = void . fork . forever $ do
  (ipAddr, var) <- atomically $ readTQueue ipLookupQueue
  res <- try (ipLookup ipAddr)
  atomically $ writeTVar var (Just res)

lookupAll :: IO ()
lookupAll = do
  glookup <- mkGeoIPLookup 10 fetchGeoIP
  withConcurrentOutput . forM_ manyIPs $ \ip -> do
    as <- lookupIP glookup (IPAddress ip)
    void . fork $ waitAsync as >>= outputConcurrent . mappend "\n" . tshow
