-- generated by rvion/jetpack-gen 
module AsC where
import Data.Conduit


c_connect :: forall (m :: * -> *) a b.
  Monad m =>
  Source m a -> Sink a m b -> m b
c_connect =  T.connect

c_fuse :: forall a (m :: * -> *) b c r.
  Monad m =>
  Conduit a m b -> ConduitM b c m r -> ConduitM a c m r
c_fuse =  T.fuse
