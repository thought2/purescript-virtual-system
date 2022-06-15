module System.Util
  ( absDir
  , absFile
  ) where

import Prelude

import Data.Maybe (fromJust)
import Pathy (AbsDir, AbsFile, parseAbsDir, parseAbsFile, posixParser)

absDir :: Partial => String -> AbsDir
absDir x = parseAbsDir posixParser x # fromJust

absFile :: Partial => String -> AbsFile
absFile x = parseAbsFile posixParser x # fromJust

