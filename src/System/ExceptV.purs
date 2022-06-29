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
import System.Class (class MonadGetCwd, class MonadLog, class MonadLogErr, class MonadReadFile, class MonadSetCwd, class MonadSystem, class MonadWriteFile, ErrReadFile, ErrWriteFile)
import System.Class as C

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

--------------------------------------------------------------------------------

writeFileLines :: forall r m. MonadWriteFile m => AbsFile -> Array String -> ExceptV (ErrWriteFile r) m Unit
writeFileLines x1 x2 = C.writeFileLines x1 x2 # ExceptT

readFileLines :: forall r m. MonadReadFile m => AbsFile -> ExceptV (ErrReadFile r) m (Array String)
readFileLines x = C.readFileLines x # ExceptT

