module Control.Concurrent.AsCtrl
  ( module Control.Concurrent.AsCtrl
  ) where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Control.Concurrent as I


-- ctrl_forkFinally :: forall a. IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
ctrl_forkFinally = I.forkFinally

-- ctrl_forkOS :: IO () -> IO ThreadId
ctrl_forkOS = I.forkOS

-- ctrl_isCurrentThreadBound :: IO Bool
ctrl_isCurrentThreadBound = I.isCurrentThreadBound

-- ctrl_rtsSupportsBoundThreads :: Bool
ctrl_rtsSupportsBoundThreads = I.rtsSupportsBoundThreads

-- ctrl_runInBoundThread :: forall a. IO a -> IO a
ctrl_runInBoundThread = I.runInBoundThread

-- ctrl_runInUnboundThread :: forall a. IO a -> IO a
ctrl_runInUnboundThread = I.runInUnboundThread

-- ctrl_threadWaitRead :: Fd -> IO ()
ctrl_threadWaitRead = I.threadWaitRead

-- ctrl_threadWaitReadSTM :: Fd -> IO (STM (), IO ())
ctrl_threadWaitReadSTM = I.threadWaitReadSTM

-- ctrl_threadWaitWrite :: Fd -> IO ()
ctrl_threadWaitWrite = I.threadWaitWrite

-- ctrl_threadWaitWriteSTM :: Fd -> IO (STM (), IO ())
ctrl_threadWaitWriteSTM = I.threadWaitWriteSTM

-- ctrl_dupChan :: forall a. Chan a -> IO (Chan a)
ctrl_dupChan = I.dupChan

-- ctrl_getChanContents :: forall a. Chan a -> IO [a]
ctrl_getChanContents = I.getChanContents

-- ctrl_isEmptyChan :: forall a. Chan a -> IO Bool
ctrl_isEmptyChan = I.isEmptyChan

-- ctrl_newChan :: forall a. IO (Chan a)
ctrl_newChan = I.newChan

-- ctrl_readChan :: forall a. Chan a -> IO a
ctrl_readChan = I.readChan

-- ctrl_unGetChan :: forall a. Chan a -> a -> IO ()
ctrl_unGetChan = I.unGetChan

-- ctrl_writeChan :: forall a. Chan a -> a -> IO ()
ctrl_writeChan = I.writeChan

-- ctrl_writeList2Chan :: forall a. Chan a -> [a] -> IO ()
ctrl_writeList2Chan = I.writeList2Chan

-- ctrl_addMVarFinalizer :: forall a. MVar a -> IO () -> IO ()
ctrl_addMVarFinalizer = I.addMVarFinalizer

-- ctrl_mkWeakMVar :: forall a. MVar a -> IO () -> IO (Weak (MVar a))
ctrl_mkWeakMVar = I.mkWeakMVar

-- ctrl_modifyMVar :: forall a b. MVar a -> (a -> IO (a, b)) -> IO b
ctrl_modifyMVar = I.modifyMVar

-- ctrl_modifyMVarMasked :: forall a b. MVar a -> (a -> IO (a, b)) -> IO b
ctrl_modifyMVarMasked = I.modifyMVarMasked

-- ctrl_modifyMVarMasked_ :: forall a. MVar a -> (a -> IO a) -> IO ()
ctrl_modifyMVarMasked_ = I.modifyMVarMasked_

-- ctrl_modifyMVar_ :: forall a. MVar a -> (a -> IO a) -> IO ()
ctrl_modifyMVar_ = I.modifyMVar_

-- ctrl_swapMVar :: forall a. MVar a -> a -> IO a
ctrl_swapMVar = I.swapMVar

-- ctrl_withMVar :: forall a b. MVar a -> (a -> IO b) -> IO b
ctrl_withMVar = I.withMVar

-- ctrl_withMVarMasked :: forall a b. MVar a -> (a -> IO b) -> IO b
ctrl_withMVarMasked = I.withMVarMasked

-- ctrl_newQSem :: Int -> IO QSem
ctrl_newQSem = I.newQSem

-- ctrl_signalQSem :: QSem -> IO ()
ctrl_signalQSem = I.signalQSem

-- ctrl_waitQSem :: QSem -> IO ()
ctrl_waitQSem = I.waitQSem

-- ctrl_newQSemN :: Int -> IO QSemN
ctrl_newQSemN = I.newQSemN

-- ctrl_signalQSemN :: QSemN -> Int -> IO ()
ctrl_signalQSemN = I.signalQSemN

-- ctrl_waitQSemN :: QSemN -> Int -> IO ()
ctrl_waitQSemN = I.waitQSemN

-- ctrl_threadDelay :: Int -> IO ()
ctrl_threadDelay = I.threadDelay

-- ctrl_forkIO :: IO () -> IO ThreadId
ctrl_forkIO = I.forkIO

-- ctrl_forkIOWithUnmask :: ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
ctrl_forkIOWithUnmask = I.forkIOWithUnmask

-- ctrl_forkOn :: Int -> IO () -> IO ThreadId
ctrl_forkOn = I.forkOn

-- ctrl_forkOnWithUnmask :: Int -> ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
ctrl_forkOnWithUnmask = I.forkOnWithUnmask

-- ctrl_getNumCapabilities :: IO Int
ctrl_getNumCapabilities = I.getNumCapabilities

-- ctrl_killThread :: ThreadId -> IO ()
ctrl_killThread = I.killThread

-- ctrl_mkWeakThreadId :: ThreadId -> IO (Weak ThreadId)
ctrl_mkWeakThreadId = I.mkWeakThreadId

-- ctrl_myThreadId :: IO ThreadId
ctrl_myThreadId = I.myThreadId

-- ctrl_setNumCapabilities :: Int -> IO ()
ctrl_setNumCapabilities = I.setNumCapabilities

-- ctrl_threadCapability :: ThreadId -> IO (Int, Bool)
ctrl_threadCapability = I.threadCapability

-- ctrl_throwTo :: forall e. Exception e => ThreadId -> e -> IO ()
ctrl_throwTo = I.throwTo

-- ctrl_yield :: IO ()
ctrl_yield = I.yield

-- ctrl_isEmptyMVar :: forall a. MVar a -> IO Bool
ctrl_isEmptyMVar = I.isEmptyMVar

-- ctrl_newEmptyMVar :: forall a. IO (MVar a)
ctrl_newEmptyMVar = I.newEmptyMVar

-- ctrl_newMVar :: forall a. a -> IO (MVar a)
ctrl_newMVar = I.newMVar

-- ctrl_putMVar :: forall a. MVar a -> a -> IO ()
ctrl_putMVar = I.putMVar

-- ctrl_readMVar :: forall a. MVar a -> IO a
ctrl_readMVar = I.readMVar

-- ctrl_takeMVar :: forall a. MVar a -> IO a
ctrl_takeMVar = I.takeMVar

-- ctrl_tryPutMVar :: forall a. MVar a -> a -> IO Bool
ctrl_tryPutMVar = I.tryPutMVar

-- ctrl_tryReadMVar :: forall a. MVar a -> IO (Maybe a)
ctrl_tryReadMVar = I.tryReadMVar

-- ctrl_tryTakeMVar :: forall a. MVar a -> IO (Maybe a)
ctrl_tryTakeMVar = I.tryTakeMVar

type CtrlChan a = I.Chan a

type CtrlQSem  = I.QSem

type CtrlQSemN  = I.QSemN

type CtrlThreadId  = I.ThreadId

type CtrlMVar a = I.MVar a
