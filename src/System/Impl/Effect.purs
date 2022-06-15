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
import System.Class (class MonadSystem, class Print, errReadFile, errWriteFile, print)

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
  log = print >>> C.log >>> wrap

  logErr = print >>> C.error >>> wrap

  getCwd = N.cwd <#> unsafePartial parseAbsDir # wrap

  setCwd x = N.chdir (printPath x) # wrap

  readFile p =
    readTextFile UTF8 (printPath p)
      # catchEffect (\e -> errReadFile { path: p, native: pure $ show e })
      # wrap

  writeFile p c =
    writeTextFile UTF8 (printPath p) c
      # catchEffect (\e -> errWriteFile { path: p, native: pure $ show e })
      # wrap

--------------------------------------------------------------------------------

catchEffect :: forall e a. (Error -> e) -> E.Effect a -> E.Effect (Either e a)
catchEffect cb m = m <#> Right # catchException (\e -> pure $ Left $ cb e)

printPath :: forall a. IsDirOrFile a => Path Abs a -> String
printPath p = P.sandboxAny p # P.printPath posixPrinter

parseAbsDir :: Partial => String -> AbsDir
parseAbsDir = P.parseAbsDir posixParser >>> fromJust