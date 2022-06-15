module System.Impl.Virtual
  ( Virtual
  , runVirtual
  ) where

import Prelude

import Control.Monad.State (State, evalState, get, modify)
import Data.Either (Either(..), note)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Pathy (Abs, Dir, File, Path)
import System.Class (class MonadSystem, class MonadVirtualSystem, GetCwd, Log, LogError, ReadFile, WriteFile, GetStdout, errReadFile)

--------------------------------------------------------------------------------

type SystemState e o =
  { stderr :: Array e
  , stdout :: Array o
  , cwd :: Path Abs Dir
  , files :: Map (Path Abs File) String
  }

newtype Virtual e o a = Virtual (State (SystemState e o) a)

--------------------------------------------------------------------------------

derive newtype instance functorVirtual :: Functor (Virtual e o)

derive newtype instance applyVirtual :: Apply (Virtual e o)

derive newtype instance applicativeVirtual :: Applicative (Virtual e o)

derive newtype instance bindVirtual :: Bind (Virtual e o)

derive newtype instance monadVirtual :: Monad (Virtual e o)

instance monadSystemVirtual :: MonadSystem e o (Virtual e o) where
  log = _log
  logErr = _logErr
  getCwd = _getCwd
  readFile = _readFile
  writeFile = _writeFile

--------------------------------------------------------------------------------
_log :: forall e o. Log o (Virtual e o)
_log msg = Virtual do
  _ <- modify (\st -> st { stdout = st.stdout <> [ msg ] })
  pure unit

_logErr :: forall e o. LogError e (Virtual e o)
_logErr msg = Virtual do
  _ <- modify (\st -> st { stderr = st.stderr <> [ msg ] })
  pure unit

_getCwd :: forall e o. GetCwd (Virtual e o)
_getCwd = Virtual do
  { cwd } <- get
  pure cwd

_readFile :: forall r e o. ReadFile r (Virtual e o)
_readFile p = Virtual do
  { files } <- get
  M.lookup p files # note (errReadFile { path: p, native: Nothing }) # pure

_writeFile :: forall r e o. WriteFile r (Virtual e o)
_writeFile p c = Virtual do
  { files } <- get
  let files' = M.insert p c files
  _ <- modify (\st -> st { files = files' })
  pure $ Right unit

--------------------------------------------------------------------------------

instance monadVirtualSystemVirtual :: MonadVirtualSystem e o (Virtual e o) where
  getStdout = Virtual do
    { stdout } <- get
    pure stdout

  getStderr = Virtual do
    { stderr } <- get
    pure stderr

--------------------------------------------------------------------------------

initSt :: forall e o. Path Abs Dir -> SystemState e o
initSt cwd =
  { stdout: []
  , stderr: []
  , cwd
  , files: M.empty
  }

runVirtual :: forall e o a. Path Abs Dir -> Virtual e o a -> a
runVirtual cwd (Virtual st) = evalState st (initSt cwd)