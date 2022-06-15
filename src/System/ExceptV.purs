module System.ExceptV
  ( getCwd
  , log
  , logErr
  , readFile
  , readFileLines
  , setCwd
  , writeFile
  , writeFileLines
  ) where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except.Checked (ExceptV)
import Data.Either (Either(..))
import Pathy (AbsDir, AbsFile)
import System.Class (class MonadSystem, ErrReadFile, ErrWriteFile)
import System.Class as C

log :: forall r e o m. MonadSystem e o m => o -> ExceptV r m Unit
log x1 = C.log x1 <#> Right # ExceptT

logErr :: forall r e o m. MonadSystem e o m => e -> ExceptV r m Unit
logErr x1 = C.logErr x1 <#> Right # ExceptT

getCwd :: forall r e o m. MonadSystem e o m => ExceptV r m AbsDir
getCwd = C.getCwd <#> Right # ExceptT

setCwd :: forall r e o m. MonadSystem e o m => AbsDir -> ExceptV r m Unit
setCwd x1 = C.setCwd x1 <#> Right # ExceptT

readFile :: forall r e o m. MonadSystem e o m => AbsFile -> ExceptV (ErrReadFile r) m String
readFile x1 = C.readFile x1 # ExceptT

writeFile :: forall r e o m. MonadSystem e o m => AbsFile -> String -> ExceptV (ErrWriteFile r) m Unit
writeFile x1 x2 = C.writeFile x1 x2 # ExceptT

--------------------------------------------------------------------------------

writeFileLines :: forall r e o m. MonadSystem e o m => AbsFile -> Array String -> ExceptV (ErrWriteFile r) m Unit
writeFileLines x1 x2 = C.writeFileLines x1 x2 # ExceptT

readFileLines :: forall r e o m. MonadSystem e o m => AbsFile -> ExceptV (ErrReadFile r) m (Array String)
readFileLines x = C.readFileLines x # ExceptT

