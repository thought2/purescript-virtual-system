module System.Class
  ( EitherV
  , class MonadGetCwd
  , class MonadLog
  , class MonadLogErr
  , class MonadReadFile
  , class MonadSetCwd
  , class MonadSystem
  , class MonadVirtualSystem
  , class MonadWriteFile
  , getCwd
  , log
  , logErr
  , readFile
  , readFileLines
  , setCwd
  , writeFile
  , writeFileLines
  ) where

import Prelude

import Data.Either (Either)
import Data.String (Pattern(..), joinWith, split)
import Data.Variant (Variant)
import Pathy (AbsDir, AbsFile)
import System.Error (ErrReadFile, ErrWriteFile)

--------------------------------------------------------------------------------

type EitherV r a = Either (Variant r) a

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

