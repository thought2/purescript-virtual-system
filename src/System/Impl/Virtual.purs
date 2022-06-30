module System.Impl.Virtual
  ( SystemState
  , Virtual
  , getStderr
  , getStdout
  , initState
  , runExceptVirtual
  , runVirtual
  ) where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.State (State, get, modify, runState)
import Data.Either (Either(..), note)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Pathy (Abs, Dir, File, Path, rootDir)
import System.Class (class MonadGetCwd, class MonadLog, class MonadLogErr, class MonadReadFile, class MonadSetCwd, class MonadSystem, class MonadVirtualSystem, class MonadWriteFile, EitherV)
import System.Error (ReadFileError(..), errReadFile)

--------------------------------------------------------------------------------

type SystemState' e o =
  { stderr :: Array e
  , stdout :: Array o
  , cwd :: Path Abs Dir
  , files :: Map (Path Abs File) String
  }

newtype SystemState e o = SystemState (SystemState' e o)

unwrapSystemState :: forall e o. SystemState e o -> SystemState' e o
unwrapSystemState (SystemState s) = s

wrapSystemState :: forall e o. SystemState' e o -> SystemState e o
wrapSystemState s = SystemState s

modifySystemState :: forall e o. (SystemState' e o -> SystemState' e o) -> SystemState e o -> SystemState e o
modifySystemState f (SystemState s) = SystemState $ f s

newtype Virtual e o a = Virtual (State (SystemState e o) a)

--------------------------------------------------------------------------------

getStderr :: forall e o. SystemState e o -> Array e
getStderr = unwrapSystemState >>> _.stderr

getStdout :: forall e o. SystemState e o -> Array o
getStdout = unwrapSystemState >>> _.stdout

--------------------------------------------------------------------------------

derive newtype instance functorVirtual :: Functor (Virtual e o)

derive newtype instance applyVirtual :: Apply (Virtual e o)

derive newtype instance applicativeVirtual :: Applicative (Virtual e o)

derive newtype instance bindVirtual :: Bind (Virtual e o)

derive newtype instance monadVirtual :: Monad (Virtual e o)

instance monadLogVirtual :: MonadLog o (Virtual e o) where
  log msg = Virtual do
    _ <- modify $ modifySystemState (\st -> st { stdout = st.stdout <> [ msg ] })
    pure unit

instance monadLogErrVirtual :: MonadLogErr e (Virtual e o) where
  logErr msg = Virtual do
    _ <- modify $ modifySystemState (\st -> st { stderr = st.stderr <> [ msg ] })
    pure unit

instance monadGetCwdVirtual :: MonadGetCwd (Virtual e o) where
  getCwd = Virtual do
    { cwd } <- get <#> unwrapSystemState
    pure cwd

instance monadSetCwdVirtual :: MonadSetCwd (Virtual e o) where
  setCwd cwd = Virtual do
    _ <- modify $ modifySystemState (\st -> st { cwd = cwd })
    pure unit

instance monadReadFileVirtual :: MonadReadFile (Virtual e o) where
  readFile p = Virtual do
    { files } <- get <#> unwrapSystemState
    M.lookup p files # note (errReadFile $ ReadFileError { path: p, native: Nothing }) # pure

instance monadWriteFileVirtual :: MonadWriteFile (Virtual e o) where
  writeFile p c = Virtual do
    { files } <- get <#> unwrapSystemState
    let files' = M.insert p c files
    _ <- modify $ modifySystemState (\st -> st { files = files' })
    pure $ Right unit

instance monadSystemVirtual :: MonadSystem e o (Virtual e o)

--------------------------------------------------------------------------------

instance monadVirtualSystemVirtual :: MonadVirtualSystem e o (Virtual e o)

--------------------------------------------------------------------------------

initState :: forall e o. SystemState e o
initState = wrapSystemState
  { stdout: []
  , stderr: []
  , cwd: rootDir
  , files: M.empty
  }

runExceptVirtual :: forall e o r a. SystemState e o -> ExceptV r (Virtual e o) a -> EitherV r a /\ SystemState e o
runExceptVirtual st m = runExceptT m # runVirtual st

runVirtual :: forall e o a. SystemState e o -> Virtual e o a -> a /\ SystemState e o
runVirtual st (Virtual m) = runState m st
