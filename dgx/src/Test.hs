module Test where
import ClassyPrelude

test :: [()]
test = return ()

aa = undefined

x a b =
  if a > 4
   then b:"ok"
   else "nope"
