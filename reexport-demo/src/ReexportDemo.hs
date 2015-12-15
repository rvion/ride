module ReexportDemo
  ( module ReexportDemo
  , Either(..)
  , module Control.Applicative
  ) where
import           Control.Applicative
type ThisIsAType = Int

type Acc a b = Yo a b

-- type YoPlop = Acc
--   reexport-demo/src/ReexportDemo.hs:6:1: KindError:
--   Type synonym ‘Acc’ should have 2 arguments, but has been given none
--   In the type declaration for ‘YoPlop’

data Yo a b = Yo { aa :: a, bb :: b, cc :: (a,b,b,a)}
data Foo = Foo1 {test :: Int} | Foo2 {test::Int, bar :: String}


{-# DEPRECATED a "Don't use a pleaz" #-}
a x y = 333


a' = b
b x y = x+y
