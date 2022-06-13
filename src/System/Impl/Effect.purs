module System.Impl.Effect
  ( Effect(..)
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (class Newtype, wrap)
import Effect as E
import Effect.Console as C
import Effect.Exception (Error, catchException)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Pathy (class IsDirOrFile, Abs, Path, posixPrinter)
import Pathy as P
import System.Class (class MonadSystem, Log, LogError, ReadFile, WriteFile, errReadFile, errWriteFile)

--------------------------------------------------------------------------------

newtype Effect a = Effect (E.Effect a)

--------------------------------------------------------------------------------

derive instance newtypeEffect :: Newtype (Effect a) _

derive newtype instance functorEffect :: Functor Effect

derive newtype instance applyEffect :: Apply Effect

derive newtype instance applicativeEffect :: Applicative Effect

derive newtype instance bindEffect :: Bind Effect

derive newtype instance monadEffect :: Monad Effect

instance monadSystemEffect :: MonadSystem Effect where
  log = _log
  logErr = _logErr
  readFile = _readFile
  writeFile = _writeFile

--------------------------------------------------------------------------------

_log :: Log Effect
_log = C.log >>> wrap

_logErr :: LogError Effect
_logErr = C.error >>> wrap

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