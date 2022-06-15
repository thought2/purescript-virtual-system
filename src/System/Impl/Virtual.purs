module System.Impl.Virtual
  ( Virtual
  , runVirtual
  ) where

import Prelude

import Control.Monad.Except (Except, runExcept)
import Control.Monad.State (StateT, evalStateT, get, modify)
import Data.Either (Either(..), note)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Pathy (Abs, Dir, File, Path, rootDir)
import System.Class (class MonadSystem, class MonadVirtualSystem, EitherV, errReadFile)

--------------------------------------------------------------------------------

type SystemState e o =
  { stderr :: Array e
  , stdout :: Array o
  , cwd :: Path Abs Dir
  , files :: Map (Path Abs File) String
  }

newtype Virtual e o r a = Virtual (StateT (SystemState e o) (Except (Variant r)) a)

--------------------------------------------------------------------------------

derive newtype instance functorVirtual :: Functor (Virtual e o r)

derive newtype instance applyVirtual :: Apply (Virtual e o r)

derive newtype instance applicativeVirtual :: Applicative (Virtual e o r)

derive newtype instance bindVirtual :: Bind (Virtual e o r)

derive newtype instance monadVirtual :: Monad (Virtual e o r)

instance monadSystemVirtual :: MonadSystem e o (Virtual e o r) where
  log msg = Virtual do
    _ <- modify (\st -> st { stdout = st.stdout <> [ msg ] })
    pure unit

  logErr msg = Virtual do
    _ <- modify (\st -> st { stderr = st.stderr <> [ msg ] })
    pure unit

  getCwd = Virtual do
    { cwd } <- get
    pure cwd

  setCwd cwd = Virtual do
    _ <- modify (\st -> st { cwd = cwd })
    pure unit

  readFile p = Virtual do
    { files } <- get
    M.lookup p files # note (errReadFile { path: p, native: Nothing }) # pure

  writeFile p c = Virtual do
    { files } <- get
    let files' = M.insert p c files
    _ <- modify (\st -> st { files = files' })
    pure $ Right unit

--------------------------------------------------------------------------------

instance monadVirtualSystemVirtual :: MonadVirtualSystem e o (Virtual e o r) where
  getStdout = Virtual do
    { stdout } <- get
    pure stdout

  getStderr = Virtual do
    { stderr } <- get
    pure stderr

--------------------------------------------------------------------------------

initSt :: forall e o. SystemState e o
initSt =
  { stdout: []
  , stderr: []
  , cwd: rootDir
  , files: M.empty
  }

runVirtual :: forall e o r a. Virtual e o r a -> EitherV r a
runVirtual (Virtual m) = evalStateT m initSt # runExcept

