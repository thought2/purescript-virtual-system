module System.Impl.Effect
  ( Effect(..)
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype, wrap)
import Effect as E
import Effect.Console as C
import Effect.Exception (Error, catchException)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Process as N
import Partial.Unsafe (unsafePartial)
import Pathy (class IsDirOrFile, Abs, Path, posixParser, posixPrinter)
import Pathy as P
import System.Class (class MonadSystem, class Print, GetCwd, Log, LogError, ReadFile, WriteFile, errReadFile, errWriteFile, print)

--------------------------------------------------------------------------------

newtype Effect a = Effect (E.Effect a)

--------------------------------------------------------------------------------

derive instance newtypeEffect :: Newtype (Effect a) _

derive newtype instance functorEffect :: Functor Effect

derive newtype instance applyEffect :: Apply Effect

derive newtype instance applicativeEffect :: Applicative Effect

derive newtype instance bindEffect :: Bind Effect

derive newtype instance monadEffect :: Monad Effect

instance monadSystemEffect :: (Print e, Print o) => MonadSystem e o Effect where
  log = _log
  logErr = _logErr
  getCwd = _getCwd
  readFile = _readFile
  writeFile = _writeFile

--------------------------------------------------------------------------------

_log :: forall o. Print o => Log o Effect
_log = print >>> C.log >>> wrap

_logErr :: forall e. Print e => LogError e Effect
_logErr = print >>> C.error >>> wrap

_getCwd :: GetCwd Effect
_getCwd = N.cwd <#> unsafePartial f # wrap
  where
    f :: Partial => _
    f = P.parseAbsDir posixParser >>> fromJust

_readFile :: forall r. ReadFile r Effect
_readFile p =
  readTextFile UTF8 (printPath p)
    # catchEffect (\e -> errReadFile { path: p, native: pure $ show e })
    # wrap

_writeFile :: forall r. WriteFile r Effect
_writeFile p c =
  writeTextFile UTF8 (printPath p) c
    # catchEffect (\e -> errWriteFile { path: p, native: pure $ show e })
    # wrap

--------------------------------------------------------------------------------

catchEffect :: forall e a. (Error -> e) -> E.Effect a -> E.Effect (Either e a)
catchEffect cb m = m <#> Right # catchException (\e -> pure $ Left $ cb e)

printPath :: forall a. IsDirOrFile a => Path Abs a -> String
printPath p = P.sandboxAny p # P.printPath posixPrinter