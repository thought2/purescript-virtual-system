module System.ExceptV
  ( getCwd
  , log
  , logErr
  , readFile
  , writeFile
  ) where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except.Checked (ExceptV)
import Data.Either (Either(..))
import Pathy (Abs, Dir, File, Path)
import System.Class (class MonadSystem, ErrReadFile, ErrWriteFile)
import System.Class as C

log :: forall r m. MonadSystem m => String -> ExceptV r m Unit
log x1 = C.log x1 <#> Right # ExceptT

logErr :: forall r m. MonadSystem m => String -> ExceptV r m Unit
logErr x1 = C.logErr x1 <#> Right # ExceptT

getCwd :: forall r m. MonadSystem m => ExceptV r m (Path Abs Dir)
getCwd = C.getCwd <#> Right # ExceptT

readFile :: forall r m. MonadSystem m => Path Abs File -> ExceptV (ErrReadFile r) m String
readFile x1 = C.readFile x1 # ExceptT

writeFile :: forall r m. MonadSystem m => Path Abs File -> String -> ExceptV (ErrWriteFile r) m Unit
writeFile x1 x2 = C.writeFile x1 x2 # ExceptT