module System.Class
  ( EitherV
  , Err
  , ErrReadFile
  , ErrWriteFile
  , ReadFileError
  , WriteFileError
  , class MonadSystem
  , class MonadVirtualSystem
  , class Print
  , errReadFile
  , errWriteFile
  , getCwd
  , setCwd
  , getStderr
  , getStdout
  , log
  , logErr
  , readFile
  , writeFile
  , print
  ) where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Variant (Variant, inj)
import Pathy (Abs, AbsDir, AbsFile, File, Path)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

--------------------------------------------------------------------------------

type EitherV r a = Either (Variant r) a

--------------------------------------------------------------------------------

type ErrReadFile r = (errReadFile :: ReadFileError | r)

type ErrWriteFile r = (errWriteFile :: WriteFileError | r)

type Err r = ErrReadFile + ErrWriteFile + r

--------------------------------------------------------------------------------

errWriteFile :: forall r. WriteFileError -> Variant (ErrWriteFile r)
errWriteFile = inj (Proxy :: _ "errWriteFile")

errReadFile :: forall r. ReadFileError -> Variant (ErrReadFile r)
errReadFile = inj (Proxy :: _ "errReadFile")

--------------------------------------------------------------------------------

type ReadFileError =
  { path :: Path Abs File
  , native :: Maybe String
  }

type WriteFileError =
  { path :: Path Abs File
  , native :: Maybe String
  }

--------------------------------------------------------------------------------

class
  Monad m <=
  MonadSystem e o m
  | m -> e o where
  log :: o -> m Unit
  logErr :: e -> m Unit
  getCwd :: m AbsDir
  setCwd :: AbsDir -> m Unit
  readFile :: forall r. AbsFile -> m (EitherV (ErrReadFile r) String)
  writeFile :: forall r. AbsFile -> String -> m (EitherV (ErrWriteFile r) Unit)

--------------------------------------------------------------------------------

class Monad m <= MonadVirtualSystem e o m | m -> e o where
  getStdout :: m (Array o)
  getStderr :: m (Array e)

--------------------------------------------------------------------------------

class Print a where
  print :: a -> String