module System.Class
  ( EitherV
  , Err
  , ErrReadFile
  , ErrWriteFile
  , ReadFileError
  , WriteFileError
  , class MonadGetCwd
  , class MonadLog
  , class MonadLogErr
  , class MonadReadFile
  , class MonadSetCwd
  , class MonadSystem
  , class MonadVirtualSystem
  , class MonadWriteFile
  , class Print
  , errReadFile
  , errWriteFile
  , getCwd
  , log
  , logErr
  , print
  , readFile
  , readFileLines
  , setCwd
  , writeFile
  , writeFileLines
  ) where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.String (Pattern(..), joinWith, split)
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

class Monad m <= MonadLog o m | m -> o where
  log :: o -> m Unit

class Monad m <= MonadLogErr e m | m -> e where
  logErr :: e -> m Unit

class Monad m <= MonadGetCwd m where
  getCwd :: m AbsDir

class Monad m <= MonadSetCwd m where
  setCwd :: AbsDir -> m Unit

class Monad m <= MonadReadFile m where
  readFile :: forall r. AbsFile -> m (EitherV (ErrReadFile r) String)

class Monad m <= MonadWriteFile m where
  writeFile :: forall r. AbsFile -> String -> m (EitherV (ErrWriteFile r) Unit)

class
  ( Monad m
  , MonadLog o m
  , MonadLogErr e m
  , MonadGetCwd m
  , MonadSetCwd m
  , MonadReadFile m
  , MonadWriteFile m
  ) <=
  MonadSystem e o m
  | m -> e o

--------------------------------------------------------------------------------

writeFileLines :: forall r m. MonadWriteFile m => AbsFile -> Array String -> m (EitherV (ErrWriteFile r) Unit)
writeFileLines x xs = writeFile x $ joinWith "\n" xs

readFileLines :: forall r m. MonadReadFile m => AbsFile -> m (EitherV (ErrReadFile r) (Array String))
readFileLines x = readFile x <#> map (split $ Pattern "\n")

--------------------------------------------------------------------------------

class MonadVirtualSystem :: forall k1 k2. k1 -> k2 -> (Type -> Type) -> Constraint
class Monad m <= MonadVirtualSystem e o m | m -> e o

--------------------------------------------------------------------------------

class Print a where
  print :: a -> String