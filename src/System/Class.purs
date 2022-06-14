module System.Class
  ( EitherV
  , Err
  , ErrReadFile
  , ErrWriteFile
  , GetCwd
  , Log
  , LogError
  , ReadFile
  , ReadFileError
  , WriteFile
  , WriteFileError
  , class MonadSystem
  , errReadFile
  , errWriteFile
  , getCwd
  , log
  , logErr
  , readFile
  , writeFile
  )
  where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Variant (Variant, inj)
import Pathy (Abs, Dir, File, Path)
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

type Log m = String -> m Unit

type LogError m = String -> m Unit

type GetCwd :: forall k. (Type -> k) -> k
type GetCwd m = m (Path Abs Dir)

type ReadFile r m = Path Abs File -> m (EitherV (ErrReadFile r) String)

type WriteFile r m = Path Abs File -> String -> m (EitherV (ErrWriteFile r) Unit)

--------------------------------------------------------------------------------

class
  Monad m <=
  MonadSystem m where
  log :: Log m
  logErr :: LogError m
  getCwd :: GetCwd m
  readFile :: forall r. ReadFile r m
  writeFile :: forall r. WriteFile r m

--------------------------------------------------------------------------------
