module System.ExceptV
  ( getCwd
  , log
  , logErr
  , readAnyFile
  , readFile
  , setCwd
  , writeAnyFile
  , writeFile
  )
  where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except.Checked (ExceptV)
import Data.Either (Either(..))
import Pathy (AbsDir, AbsFile, AnyFile)
import System.Class (class MonadGetCwd, class MonadLog, class MonadLogErr, class MonadReadFile, class MonadSetCwd, class MonadWriteFile)
import System.Class as C
import System.Error (ErrReadFile, ErrWriteFile)

log :: forall r o m. MonadLog o m => o -> ExceptV r m Unit
log x1 = C.log x1 <#> Right # ExceptT

logErr :: forall r e m. MonadLogErr e m => e -> ExceptV r m Unit
logErr x1 = C.logErr x1 <#> Right # ExceptT

getCwd :: forall r m. MonadGetCwd m => ExceptV r m AbsDir
getCwd = C.getCwd <#> Right # ExceptT

setCwd :: forall r m. MonadSetCwd m => AbsDir -> ExceptV r m Unit
setCwd x1 = C.setCwd x1 <#> Right # ExceptT

readFile :: forall r m. MonadReadFile m => AbsFile -> ExceptV (ErrReadFile r) m String
readFile x1 = C.readFile x1 # ExceptT

writeFile :: forall r m. MonadWriteFile m => AbsFile -> String -> ExceptV (ErrWriteFile r) m Unit
writeFile x1 x2 = C.writeFile x1 x2 # ExceptT

readAnyFile :: forall r m. MonadGetCwd m => MonadReadFile m => AnyFile -> ExceptV (ErrReadFile r) m String
readAnyFile x1 = C.readAnyFile x1 # ExceptT

writeAnyFile :: forall r m. MonadGetCwd m => MonadWriteFile m => AnyFile -> String -> ExceptV (ErrWriteFile r) m Unit
writeAnyFile x1 x2 = C.writeAnyFile x1 x2 # ExceptT
