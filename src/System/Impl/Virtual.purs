module System.Impl.Virtual
  ( SystemState
  , Virtual
  , getStderr
  , getStdout
  , runVirtual
  ) where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (State, get, modify, runState)
import Data.Either (Either(..), note)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant)
import Pathy (Abs, Dir, File, Path, rootDir)
import System.Class (class MonadGetCwd, class MonadLog, class MonadLogErr, class MonadReadFile, class MonadSetCwd, class MonadSystem, class MonadVirtualSystem, class MonadWriteFile, EitherV, errReadFile)

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

newtype Virtual e o r a = Virtual (ExceptT (Variant r) (State (SystemState e o)) a)

--------------------------------------------------------------------------------

getStderr :: forall e o. SystemState e o -> Array e
getStderr = unwrapSystemState >>> _.stderr

getStdout :: forall e o. SystemState e o -> Array o
getStdout = unwrapSystemState >>> _.stdout

--------------------------------------------------------------------------------

derive newtype instance functorVirtual :: Functor (Virtual e o r)

derive newtype instance applyVirtual :: Apply (Virtual e o r)

derive newtype instance applicativeVirtual :: Applicative (Virtual e o r)

derive newtype instance bindVirtual :: Bind (Virtual e o r)

derive newtype instance monadVirtual :: Monad (Virtual e o r)

instance monadLogVirtual :: MonadLog o (Virtual e o r) where
  log msg = Virtual do
    _ <- modify $ modifySystemState (\st -> st { stdout = st.stdout <> [ msg ] })
    pure unit

instance monadLogErrVirtual :: MonadLogErr e (Virtual e o r) where
  logErr msg = Virtual do
    _ <- modify $ modifySystemState (\st -> st { stderr = st.stderr <> [ msg ] })
    pure unit

instance monadGetCwdVirtual :: MonadGetCwd (Virtual e o r) where
  getCwd = Virtual do
    { cwd } <- get <#> unwrapSystemState
    pure cwd

instance monadSetCwdVirtual :: MonadSetCwd (Virtual e o r) where
  setCwd cwd = Virtual do
    _ <- modify $ modifySystemState (\st -> st { cwd = cwd })
    pure unit

instance monadReadFileVirtual :: MonadReadFile (Virtual e o r) where
  readFile p = Virtual do
    { files } <- get <#> unwrapSystemState
    M.lookup p files # note (errReadFile { path: p, native: Nothing }) # pure

instance monadWriteFileVirtual :: MonadWriteFile (Virtual e o r) where
  writeFile p c = Virtual do
    { files } <- get <#> unwrapSystemState
    let files' = M.insert p c files
    _ <- modify $ modifySystemState (\st -> st { files = files' })
    pure $ Right unit

instance monadSystemVirtual :: MonadSystem e o (Virtual e o r)

--------------------------------------------------------------------------------

instance monadVirtualSystemVirtual :: MonadVirtualSystem e o (Virtual e o r)

--------------------------------------------------------------------------------

initSt :: forall e o. SystemState e o
initSt = wrapSystemState
  { stdout: []
  , stderr: []
  , cwd: rootDir
  , files: M.empty
  }

runVirtual :: forall e o r a. Virtual e o r a -> EitherV r a /\ SystemState e o
runVirtual (Virtual m) = runExceptT m # flip runState initSt

