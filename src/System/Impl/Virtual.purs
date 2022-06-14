module System.Impl.Virtual
  ( Virtual
  , getStderr
  , getStdout
  , runVirtual
  )
  where

import Prelude

import Control.Monad.State (State, evalState, get, modify)
import Data.Either (Either(..), note)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Pathy (Abs, Dir, File, Path)
import System.Class (class MonadSystem, Log, LogError, ReadFile, WriteFile, GetCwd, errReadFile)

--------------------------------------------------------------------------------

type SystemState =
  { stdout :: String
  , stderr :: String
  , cwd :: Path Abs Dir
  , files :: Map (Path Abs File) String
  }

newtype Virtual a = Virtual (State SystemState a)

--------------------------------------------------------------------------------

derive newtype instance functorVirtual :: Functor Virtual

derive newtype instance applyVirtual :: Apply Virtual

derive newtype instance applicativeVirtual :: Applicative Virtual

derive newtype instance bindVirtual :: Bind Virtual

derive newtype instance monadVirtual :: Monad Virtual

instance monadSystemVirtual :: MonadSystem Virtual where
  log = _log
  logErr = _logErr
  getCwd = _getCwd
  readFile = _readFile
  writeFile = _writeFile

--------------------------------------------------------------------------------
_log :: Log Virtual
_log msg = Virtual do
  _ <- modify (\st -> st { stdout = st.stdout <> "\n" <> msg })
  pure unit

_logErr :: LogError Virtual
_logErr msg = Virtual do
  _ <- modify (\st -> st { stderr = st.stderr <> "\n" <> msg })
  pure unit

_getCwd :: GetCwd Virtual
_getCwd = Virtual do
  { cwd } <- get
  pure cwd

_readFile :: forall r. ReadFile r Virtual
_readFile p = Virtual do
  { files } <- get
  M.lookup p files # note (errReadFile { path: p, native: Nothing }) # pure

_writeFile :: forall r. WriteFile r Virtual
_writeFile p c = Virtual do
  { files } <- get
  let files' = M.insert p c files
  _ <- modify (\st -> st { files = files' })
  pure $ Right unit

--------------------------------------------------------------------------------

getStdout :: Virtual String
getStdout = Virtual do
  { stdout } <- get
  pure stdout

getStderr :: Virtual String
getStderr = Virtual do
  { stderr } <- get
  pure stderr

--------------------------------------------------------------------------------

initSt :: Path Abs Dir -> SystemState
initSt cwd =
  { stdout: ""
  , stderr: ""
  , cwd
  , files: M.empty
  }

runVirtual :: forall a. Path Abs Dir -> Virtual a -> a
runVirtual cwd (Virtual st) = evalState st (initSt cwd)