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
  )
  where

import Prelude

import Data.Maybe (Maybe)
import Data.Variant (Variant, inj, on)
import Pathy (Abs, File, Path)
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

newtype WriteFileError = WriteFileError
  { path :: Path Abs File
  , native :: Maybe String
  }

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
  print (ReadFileError { path }) = "Failed to read file at path " <> show path <> "."

instance printWriteFileError :: Print WriteFileError where
  print (WriteFileError { path }) = "Failed to write to file at path " <> show path <> "."