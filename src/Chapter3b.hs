{-# LANGUAGE RecordWildCards #-}

module Chapter3b
  ( JobResult
  , LookupCache(..)
  , GeoIPLookup(..)
  , mkGeoIPLookup
  , mapCache
  , lookupIP
  ) where

import           ClassyPrelude
import           System.Console.Concurrent

import           Chapter1
import           IPs

-- | Because IO actions can throw exceptions we have to deal with that
--   at some point. I decided to not encode failure into LookupResult.
type JobResult = Either SomeException LookupResult

data LookupCache = LookupCache
  { lcInsert :: IPAddress -> LookupResult -> STM ()
  , lcLookup :: IPAddress -> STM (Maybe LookupResult)
  }

-- | A data type holding the queue and lookup implementation.
data GeoIPLookup = GeoIPLookup
  { ipLookup      :: IPAddress -> IO LookupResult
  , ipLookupQueue :: TQueue (IPAddress, TVar (Maybe JobResult))
  , ipWorkLog     :: TVar (Map IPAddress (TVar (Maybe JobResult)))
  , ipCache       :: LookupCache
  }

-- | Creates a new lookup with n amount of worker threads
mkGeoIPLookup :: Int -> (IPAddress -> IO LookupResult) -> Maybe LookupCache -> IO GeoIPLookup
mkGeoIPLookup n f lc = do
  glookup <- GeoIPLookup <$> pure f <*> newTQueueIO <*> newTVarIO mempty <*> pure (fromMaybe nullCache lc)
  processQueue glookup n
  return glookup

nullCache :: LookupCache
nullCache =
  LookupCache
  {lcInsert = const . const $ return (), lcLookup = const . return $ Nothing}

mapCache :: TVar (Map IPAddress LookupResult) -> LookupCache
mapCache var =
  LookupCache
  { lcInsert = \ip res -> modifyTVar' var (insertMap ip res)
  , lcLookup = \ip -> lookup ip <$> readTVar var
  }

-- | New api for looking up IPs.
lookupIP :: GeoIPLookup -> IPAddress -> IO (Async JobResult)
lookupIP l ipAddr = async $ do
  var <- atomically (checkWorkLog <||> checkCache >>= maybe appendAddress return)
  atomically (readTVar var >>= maybe retrySTM return)

  where
    (<||>) :: STM (Maybe a) -> STM (Maybe a) -> STM (Maybe a)
    sa1 <||> sa2 = sa1 >>= maybe sa2 (return . return)

    checkCache :: STM (Maybe (TVar (Maybe JobResult)))
    checkCache = do
      x <- (lcLookup . ipCache $ l) ipAddr
      case x of
        Nothing -> return Nothing
        Just a -> do
          var <- newTVar (Just (Right a))
          return (Just var)

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
    either (const . return $ ()) (lcInsert ipCache ipAddr) res
    modifyTVar' ipWorkLog (deleteMap ipAddr)

lookupSame :: IO ()
lookupSame = do
  glookup <- mkGeoIPLookup 10 (\ip -> outputConcurrent ("Making a request\n" :: Text) >> fetchGeoIP ip) Nothing
  withConcurrentOutput . forM_ someIPsIntervened $ \ip -> do
    as <- lookupIP glookup (IPAddress ip)
    void . fork $ waitAsync as >>= outputConcurrent . mappend "\n" . tshow

lookupSameCached :: IO ()
lookupSameCached = do
  cache <- mapCache <$> newTVarIO (singletonMap "37.49.155.108" (LookupResult "our" "cache"))
  glookup <- mkGeoIPLookup 10 (\ip -> outputConcurrent ("Making a request\n" :: Text) >> fetchGeoIP ip) (Just cache)
  withConcurrentOutput . forM_ someIPsIntervened $ \ip -> do
    as <- lookupIP glookup (IPAddress ip)
    void . fork $ waitAsync as >>= outputConcurrent . mappend "\n" . tshow

lookupAll :: IO ()
lookupAll = do
  glookup <- mkGeoIPLookup 10 fetchGeoIP Nothing
  withConcurrentOutput . forM_ manyIPs $ \ip -> do
    as <- lookupIP glookup (IPAddress ip)
    void . fork $ waitAsync as >>= outputConcurrent . mappend "\n" . tshow
