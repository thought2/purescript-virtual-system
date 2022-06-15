module System.Class
  ( EitherV
  , Err
  , ErrReadFile
  , ErrWriteFile
  , GetCwd
  , GetStderr
  , GetStdout
  , Log
  , LogError
  , ReadFile
  , ReadFileError
  , WriteFile
  , WriteFileError
  , class MonadSystem
  , class MonadVirtualSystem
  , class Print
  , errReadFile
  , errWriteFile
  , getCwd
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

type Log o m = o -> m Unit

type LogError e m = e -> m Unit

type GetCwd :: forall k. (Type -> k) -> k
type GetCwd m = m (Path Abs Dir)

type ReadFile r m = Path Abs File -> m (EitherV (ErrReadFile r) String)

type WriteFile r m = Path Abs File -> String -> m (EitherV (ErrWriteFile r) Unit)

class
  Monad m <=
  MonadSystem e o m
  | m -> e o where
  log :: Log o m
  logErr :: LogError e m
  getCwd :: GetCwd m
  readFile :: forall r. ReadFile r m
  writeFile :: forall r. WriteFile r m

--------------------------------------------------------------------------------

type GetStdout :: forall k. Type -> (Type -> k) -> k
type GetStdout o m = m (Array o)

type GetStderr :: forall k. Type -> (Type -> k) -> k
type GetStderr e m = m (Array e)

class Monad m <= MonadVirtualSystem e o m | m -> e o where
  getStdout :: GetStdout o m
  getStderr :: GetStderr e m

--------------------------------------------------------------------------------

class Print a where
  print :: a -> String