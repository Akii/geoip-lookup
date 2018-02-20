{-# LANGUAGE RecordWildCards #-}

module Chapter3
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
  , ipWorkLog :: TVar (Map IPAddress (TVar (Maybe JobResult)))
  }

-- | Creates a new lookup with n amount of worker threads
mkGeoIPLookup :: Int -> (IPAddress -> IO LookupResult) -> IO GeoIPLookup
mkGeoIPLookup n f = do
  glookup <- GeoIPLookup <$> pure f <*> newTQueueIO <*> newTVarIO mempty
  processQueue glookup n
  return glookup

-- | New api for looking up IPs.
lookupIP :: GeoIPLookup -> IPAddress -> IO (Async JobResult)
lookupIP l ipAddr = async $ do
  var <- atomically (checkWorkLog >>= maybe appendAddress return)
  atomically (readTVar var >>= maybe retrySTM return)

  where
    checkWorkLog :: STM (Maybe (TVar (Maybe JobResult)))
    checkWorkLog = lookup ipAddr <$> readTVar (ipWorkLog l)

    appendAddress :: STM (TVar (Maybe JobResult))
    appendAddress = do
      var <- newTVar Nothing
      writeTQueue (ipLookupQueue l) (ipAddr, var)
      return var

-- | Just spawns a bunch of worker threads
processQueue :: GeoIPLookup -> Int -> IO ()
processQueue l n = replicateM_ n (worker l)

-- | A worker is just taking the next IP out of the queue and looks it up
worker :: GeoIPLookup -> IO ()
worker GeoIPLookup {..} = void . fork . forever $ do
  (ipAddr, var) <- atomically $ do
    next <- readTQueue ipLookupQueue
    modifyTVar' ipWorkLog (uncurry insertMap next)
    return next

  res <- try (ipLookup ipAddr)

  atomically $ do
    writeTVar var (Just res)
    modifyTVar' ipWorkLog (deleteMap ipAddr)

lookupSame :: IO ()
lookupSame = do
  glookup <- mkGeoIPLookup 10 (\ip -> outputConcurrent ("Making a request\n" :: Text) >> fetchGeoIP ip)
  withConcurrentOutput . forM_ sameIP10Times $ \ip -> do
    as <- lookupIP glookup (IPAddress ip)
    void . fork $ waitAsync as >>= outputConcurrent . mappend "\n" . tshow

lookupAll :: IO ()
lookupAll = do
  glookup <- mkGeoIPLookup 10 fetchGeoIP
  withConcurrentOutput . forM_ manyIPs $ \ip -> do
    as <- lookupIP glookup (IPAddress ip)
    void . fork $ waitAsync as >>= outputConcurrent . mappend "\n" . tshow
