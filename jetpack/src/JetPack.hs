-- |
-- This module reexports most of the definitions from the \"base\" package,
-- which are meant to be imported unqualified.
--
-- For details check out the source.
module Jetpack
  ( module X
  ) where

import           BasePrelude as X
import           Exports     as X hiding (try, uncons)
