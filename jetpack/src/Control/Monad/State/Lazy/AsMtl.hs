module Control.Monad.State.Lazy.AsMtl
  ( -- unqualified class re-export
  I.MonadState(I.get, I.put, I.state)
  , module Control.Monad.State.Lazy.AsMtl
  ) where
-- generated by https://github.com/rvion/ride/tree/master/jetpack-gen

import qualified Control.Monad.State.Lazy as I


-- mtl_filterM :: forall a (m :: * -> *). Monad m => (a -> m Bool) -> [a] -> m [a]
mtl_filterM = I.filterM

-- mtl_foldM :: forall b a (m :: * -> *) (t :: * -> *). (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
mtl_foldM = I.foldM

-- mtl_foldM_ :: forall b a (m :: * -> *) (t :: * -> *). (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m ()
mtl_foldM_ = I.foldM_

-- mtl_forever :: forall (m :: * -> *) a b. Monad m => m a -> m b
mtl_forever = I.forever

-- mtl_guard :: forall (f :: * -> *). Alternative f => Bool -> f ()
mtl_guard = I.guard

-- mtl_mapAndUnzipM :: forall a (m :: * -> *) b c. Monad m => (a -> m (b, c)) -> [a] -> m ([b], [c])
mtl_mapAndUnzipM = I.mapAndUnzipM

-- mtl_mfilter :: forall a (m :: * -> *). MonadPlus m => (a -> Bool) -> m a -> m a
mtl_mfilter = I.mfilter

-- mtl_replicateM :: forall (m :: * -> *) a. Monad m => Int -> m a -> m [a]
mtl_replicateM = I.replicateM

-- mtl_replicateM_ :: forall (m :: * -> *) a. Monad m => Int -> m a -> m ()
mtl_replicateM_ = I.replicateM_

-- mtl_unless :: forall (f :: * -> *). Applicative f => Bool -> f () -> f ()
mtl_unless = I.unless

-- mtl_zipWithM :: forall a b (m :: * -> *) c. Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
mtl_zipWithM = I.zipWithM

-- mtl_zipWithM_ :: forall a b (m :: * -> *) c. Monad m => (a -> b -> m c) -> [a] -> [b] -> m ()
mtl_zipWithM_ = I.zipWithM_

-- mtl_forM_ :: forall (t :: * -> *) a (m :: * -> *) b. (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
mtl_forM_ = I.forM_

-- mtl_mapM_ :: forall a (m :: * -> *) b (t :: * -> *). (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mtl_mapM_ = I.mapM_

-- mtl_msum :: forall (t :: * -> *) (m :: * -> *) a. (Foldable t, MonadPlus m) => t (m a) -> m a
mtl_msum = I.msum

-- mtl_sequence_ :: forall (t :: * -> *) (m :: * -> *) a. (Foldable t, Monad m) => t (m a) -> m ()
mtl_sequence_ = I.sequence_

-- mtl_fix :: forall a. (a -> a) -> a
mtl_fix = I.fix

-- mtl_void :: forall (f :: * -> *) a. Functor f => f a -> f ()
mtl_void = I.void

-- mtl_forM :: forall (t :: * -> *) a (m :: * -> *) b. (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
mtl_forM = I.forM

-- mtl_ap :: forall (m :: * -> *) a b. Monad m => m (a -> b) -> m a -> m b
mtl_ap = I.ap

-- mtl_join :: forall (m :: * -> *) a. Monad m => m (m a) -> m a
mtl_join = I.join

-- mtl_liftM :: forall a1 r (m :: * -> *). Monad m => (a1 -> r) -> m a1 -> m r
mtl_liftM = I.liftM

-- mtl_liftM2 :: forall a1 a2 r (m :: * -> *). Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
mtl_liftM2 = I.liftM2

-- mtl_liftM3 :: forall a1 a2 a3 r (m :: * -> *). Monad m => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
mtl_liftM3 = I.liftM3

-- mtl_liftM4 :: forall a1 a2 a3 a4 r (m :: * -> *). Monad m => (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r
mtl_liftM4 = I.liftM4

-- mtl_liftM5 :: forall a1 a2 a3 a4 a5 r (m :: * -> *). Monad m => (a1 -> a2 -> a3 -> a4 -> a5 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r
mtl_liftM5 = I.liftM5

-- mtl_when :: forall (f :: * -> *). Applicative f => Bool -> f () -> f ()
mtl_when = I.when

-- mtl_gets :: forall s a (m :: * -> *). MonadState s m => (s -> a) -> m a
mtl_gets = I.gets

-- mtl_modify :: forall s (m :: * -> *). MonadState s m => (s -> s) -> m ()
mtl_modify = I.modify

-- mtl_modify' :: forall s (m :: * -> *). MonadState s m => (s -> s) -> m ()
mtl_modify' = I.modify'

-- mtl_evalState :: forall s a. State s a -> s -> a
mtl_evalState = I.evalState

-- mtl_evalStateT :: forall s (m :: * -> *) a. Monad m => StateT s m a -> s -> m a
mtl_evalStateT = I.evalStateT

-- mtl_execState :: forall s a. State s a -> s -> s
mtl_execState = I.execState

-- mtl_execStateT :: forall s (m :: * -> *) a. Monad m => StateT s m a -> s -> m s
mtl_execStateT = I.execStateT

-- mtl_mapState :: forall a s b. ((a, s) -> (b, s)) -> State s a -> State s b
mtl_mapState = I.mapState

-- mtl_mapStateT :: forall (m :: * -> *) a s (n :: * -> *) b. (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mtl_mapStateT = I.mapStateT

-- mtl_runState :: forall s a. State s a -> s -> (a, s)
mtl_runState = I.runState

-- mtl_withState :: forall s a. (s -> s) -> State s a -> State s a
mtl_withState = I.withState

-- mtl_withStateT :: forall s (m :: * -> *) a. (s -> s) -> StateT s m a -> StateT s m a
mtl_withStateT = I.withStateT

type MtlState a = I.State a

type MtlStateT a b c = I.StateT a b c
