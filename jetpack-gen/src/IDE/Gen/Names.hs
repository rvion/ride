module IDE.Gen.Names where

import Data.List
import Data.Char

toN (prefix, mod) = concat [mod, ".", "As", _typePrefix]
  where
    (p:ps) = prefix
    _idPrefix = (toLower p : ps)
    _typePrefix = (toUpper p : ps)


