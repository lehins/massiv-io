{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
-- |
-- Module      : Data.Array.Massiv.Scheduler
-- Copyright   : (c) Alexey Kuleshevich 2017
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Array.Massiv.Scheduler
  ( Scheduler
  , makeScheduler
  , numWorkers
  , JobRequest(..)
  , JobResult(..)
  , submitRequest
  , collectResults
  , waitTillDone
  , splitWork
  , splitWork_
  ) where

import           Control.Concurrent             (ThreadId, forkOn,
                                                 getNumCapabilities, killThread)
import           Control.Concurrent.STM.TChan   (TChan, isEmptyTChan, newTChan,
                                                 newTChanIO, readTChan,
                                                 tryReadTChan, writeTChan)
import           Control.Concurrent.STM.TVar    (TVar, modifyTVar', newTVar,
                                                 newTVarIO, readTVar, writeTVar)
import           Control.Exception.Safe         (Exception, SomeException,
                                                 catchAny, throw, throwM)
import           Control.Monad                  (unless, void, when)
import           Control.Monad.STM              (STM, atomically)
import           Data.Array.Massiv.Common.Index
import           Data.Either                    (isLeft)
import           System.IO.Unsafe               (unsafePerformIO)

data Job = Job (Int -> IO ())
         | Retire


data SchedulerRetired = SchedulerRetired deriving Show

instance Exception SchedulerRetired

data Scheduler a = Scheduler
  { resultsChan       :: TChan (Either SomeException (JobResult a))
  , jobCountVar       :: TVar Int
  , jobQueue          :: TChan Job
  , workers           :: [ThreadId]
  , numWorkers        :: !Int
  , retiredVar        :: TVar Bool
  , isGlobalScheduler :: Bool
  }

data JobResult a = JobResult { jobResultId :: !Int
                             , jobResult   :: !a }


data JobRequest a = JobRequest { jobRequestId     :: !Int
                               , jobRequestAction :: IO a }


-- | Create a `Scheduler` that can be used to submit `JobRequest`s and collect
-- work done by the workers using `collectResults`.
makeScheduler :: IO (Scheduler a)
makeScheduler = do
  isGlobalScheduler <-
    atomically $ do
      hasGlobalScheduler <- readTVar hasGlobalSchedulerVar
      unless hasGlobalScheduler $ writeTVar hasGlobalSchedulerVar True
      return $ not hasGlobalScheduler
  (workers, jobQueue) <-
    if isGlobalScheduler
      then atomically $ readTVar globalJobQueue
      else makeJobQueue
  let numWorkers = length workers
  atomically $ do
    resultsChan <- newTChan
    jobCountVar <- newTVar 0
    retiredVar <- newTVar False
    return $ Scheduler {..}


-- | Clear out outstanding jobs in the queue
clearJobQueue :: Scheduler a -> STM ()
clearJobQueue scheduler@(Scheduler {..}) = do
  mJob <- tryReadTChan jobQueue
  case mJob of
    Just _ -> do
      modifyTVar' jobCountVar (subtract 1)
      clearJobQueue scheduler
    Nothing -> return ()


-- | Submit a `JobRequest`, which will get executed as soon as there is an
-- available worker.
submitRequest :: Scheduler a -> JobRequest a -> IO ()
submitRequest Scheduler {..} JobRequest {..} = do
  atomically $ do
    isRetired <- readTVar retiredVar
    when isRetired $ throwM SchedulerRetired
    modifyTVar' jobCountVar (+ 1)
    writeTChan jobQueue $
      Job $ \_wid -> do
        eResult <- catchAny (Right <$> jobRequestAction) (return . Left)
        atomically $ do
          modifyTVar' jobCountVar (subtract 1)
          writeTChan resultsChan (JobResult jobRequestId <$> eResult)


-- | Block current thread and wait for all `JobRequest`s to get processed. Use a
-- supplied function to collect the results, which are produced by submitted
-- jobs. If any job throws an exception, the whole scheduler is retired
-- while cancelling all other jobs, in case there are any in progress, and that
-- exception is further re-thrown.
collectResults :: Scheduler a -> (JobResult a -> b -> b) -> b -> IO b
collectResults scheduler@(Scheduler {..}) f initAcc = collect initAcc
  where
    collect !acc = do
      (eJRes, stop) <-
        atomically $ do
          isRetired <- readTVar retiredVar
          when isRetired $ throwM SchedulerRetired
          eJRes <- readTChan resultsChan
          -- prevent any new jobs from starting in case of an exception
          when (isLeft eJRes) $ do
            clearJobQueue scheduler
            writeTVar retiredVar True
          resEmpty <- isEmptyTChan resultsChan
          if resEmpty
            then do
              jCount <- readTVar jobCountVar
              return (eJRes, jCount == 0)
            else return (eJRes, False)
      case eJRes of
        Right jRes ->
          if stop
            then do
              atomically $ do
                writeTVar retiredVar True
                if isGlobalScheduler
                  then writeTVar hasGlobalSchedulerVar False
                  else loopM_ 0 (< numWorkers) (+ 1) $ \ !_ ->
                         writeTChan jobQueue Retire
              return $! f jRes acc
            else collect $ f jRes acc
        Left exc -> do
          mapM_ killThread workers
          -- kill all workers and create new ones if those were primary workers
          when isGlobalScheduler $ do
            jQueue <- makeJobQueue
            atomically $ do
              writeTVar hasGlobalSchedulerVar False
              writeTVar globalJobQueue jQueue
          throw exc


-- | Block current thread and wait for the `Scheduler` to process all submitted `JobRequest`s.
waitTillDone :: Scheduler a -> IO ()
waitTillDone scheduler = collectResults scheduler (const id) ()



splitWork :: Index ix
          => ix -> (Scheduler a -> Int -> Int -> Int -> IO b) -> IO [JobResult a]
splitWork !sz submitWork
  | totalElem sz == 0 = return []
  | otherwise = do
    scheduler <- makeScheduler
    let !totalLength = totalElem sz
        !chunkLength = totalLength `quot` numWorkers scheduler
        !slackStart = chunkLength * numWorkers scheduler
    void $ submitWork scheduler chunkLength totalLength slackStart
    collectResults scheduler (:) []

splitWork_ :: Index ix
          => ix -> (Scheduler a -> Int -> Int -> Int -> IO b) -> IO ()
splitWork_ sz = void . splitWork sz

hasGlobalSchedulerVar :: TVar Bool
hasGlobalSchedulerVar = unsafePerformIO $ newTVarIO False
{-# NOINLINE hasGlobalSchedulerVar #-}


globalJobQueue :: TVar ([ThreadId], TChan Job)
globalJobQueue = unsafePerformIO $ makeJobQueue >>= newTVarIO
{-# NOINLINE globalJobQueue #-}


makeJobQueue :: IO ([ThreadId], TChan Job)
makeJobQueue = do
  nWorkers <- getNumWorkers
  jQueue <- newTChanIO
  workers <- startWorkers jQueue nWorkers
  return (workers, jQueue)


getNumWorkers :: IO Int
getNumWorkers = getNumCapabilities


runWorker :: TChan Job -> Int -> IO ()
runWorker jQueue wid = do
  job <- atomically $ readTChan jQueue
  case job of
    Job action -> do
      action wid
      runWorker jQueue wid
    Retire -> return ()


startWorkers :: TChan Job -> Int -> IO [ThreadId]
startWorkers jQueue nWorkers =
  loopM 0 (< nWorkers) (+ 1) [] $ \ !wid acc -> do
    tid <- forkOn wid (runWorker jQueue wid)
    return (tid:acc)

