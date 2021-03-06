module System.Error
  ( Err
  , ErrReadFile
  , ErrWriteFile
  , ReadFileError(..)
  , WriteFileError(..)
  , errReadFile
  , errWriteFile
  , printErr
  , printErrReadFile
  , printErrWriteFile
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Variant (Variant, inj, on)
import Pathy (class IsDirOrFile, Abs, File, Path, posixPrinter)
import Pathy as P
import Print.Class (class Print, print)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

--------------------------------------------------------------------------------

type ErrReadFile r = (errReadFile :: ReadFileError | r)

type ErrWriteFile r = (errWriteFile :: WriteFileError | r)

type Err r = ErrReadFile + ErrWriteFile + r

--------------------------------------------------------------------------------

errWriteFile :: forall r. WriteFileError -> Variant (ErrWriteFile r)
errWriteFile = inj _errWriteFile

errReadFile :: forall r. ReadFileError -> Variant (ErrReadFile r)
errReadFile = inj _errReadFile

--------------------------------------------------------------------------------

newtype ReadFileError = ReadFileError
  { path :: Path Abs File
  , native :: Maybe String
  }

derive newtype instance showReadFileError :: Show ReadFileError
derive newtype instance eqReadFileError :: Eq ReadFileError

newtype WriteFileError = WriteFileError
  { path :: Path Abs File
  , native :: Maybe String
  }

derive newtype instance showWriteFileError :: Show WriteFileError
derive newtype instance eqwriteFileError :: Eq WriteFileError

--------------------------------------------------------------------------------

_errReadFile :: Proxy "errReadFile"
_errReadFile = Proxy

_errWriteFile :: Proxy "errWriteFile"
_errWriteFile = Proxy

--------------------------------------------------------------------------------

printErrReadFile :: forall r. (Variant r -> String) -> Variant (ErrReadFile r) -> String
printErrReadFile = on _errReadFile print

printErrWriteFile :: forall r. (Variant r -> String) -> Variant (ErrWriteFile r) -> String
printErrWriteFile = on _errWriteFile print

printErr :: forall r. (Variant r -> String) -> Variant (Err r) -> String
printErr x = x
  # printErrWriteFile
  # printErrReadFile

--------------------------------------------------------------------------------

instance printReadFileError :: Print ReadFileError where
  print (ReadFileError { path }) = "Failed to read file at path " <> printPath path <> "."

instance printWriteFileError :: Print WriteFileError where
  print (WriteFileError { path }) = "Failed to write to file at path " <> printPath path <> "."

--------------------------------------------------------------------------------
printPath :: forall a. IsDirOrFile a => Path Abs a -> String
printPath p = P.sandboxAny p # P.printPath posixPrinter
