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
import Pathy (class IsDirOrFile, Abs, Path, AbsDir, posixParser, posixPrinter)
import Pathy as P
import System.Class (class MonadGetCwd, class MonadLog, class MonadLogErr, class MonadReadFile, class MonadSetCwd, class MonadSystem, class MonadWriteFile, class Print, errReadFile, errWriteFile, print)

--------------------------------------------------------------------------------

newtype Effect a = Effect (E.Effect a)

--------------------------------------------------------------------------------

derive instance newtypeEffect :: Newtype (Effect a) _

derive newtype instance functorEffect :: Functor Effect

derive newtype instance applyEffect :: Apply Effect

derive newtype instance applicativeEffect :: Applicative Effect

derive newtype instance bindEffect :: Bind Effect

derive newtype instance monadEffect :: Monad Effect

instance monadLogEffect :: (Print o) => MonadLog o Effect where
  log = print >>> C.log >>> wrap

instance monadLogErrEffect :: (Print e) => MonadLogErr e Effect where
  logErr = print >>> C.error >>> wrap

instance monadGetCwdEffect :: MonadGetCwd Effect where
  getCwd = N.cwd <#> unsafePartial parseAbsDir # wrap

instance monadSetCwdEffect :: MonadSetCwd Effect where
  setCwd x = N.chdir (printPath x) # wrap

instance monadReadFileEffect :: MonadReadFile Effect where
  readFile p =
    readTextFile UTF8 (printPath p)
      # catchEffect (\e -> errReadFile { path: p, native: pure $ show e })
      # wrap

instance monadWriteFileEffect :: MonadWriteFile Effect where
  writeFile p c =
    writeTextFile UTF8 (printPath p) c
      # catchEffect (\e -> errWriteFile { path: p, native: pure $ show e })
      # wrap

instance monadSystemEffect :: (Print e, Print o) => MonadSystem e o Effect

--------------------------------------------------------------------------------

catchEffect :: forall e a. (Error -> e) -> E.Effect a -> E.Effect (Either e a)
catchEffect cb m = m <#> Right # catchException (\e -> pure $ Left $ cb e)

printPath :: forall a. IsDirOrFile a => Path Abs a -> String
printPath p = P.sandboxAny p # P.printPath posixPrinter

parseAbsDir :: Partial => String -> AbsDir
parseAbsDir = P.parseAbsDir posixParser >>> fromJust