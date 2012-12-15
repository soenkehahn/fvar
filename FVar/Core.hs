{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language FlexibleInstances #-}

-- | This is the core of the FVar library. Everything else should be
-- implemented in terms of this module. This is also the module that
-- needs to be tested extensively.

module FVar.Core (
    -- * construction and reading
    FVar(..),
    newFVar,
    openFVar,
    readFVar,
    existsFVar,

    -- * synchronized writing access
    -- $syncdocs
    FVars(..),
    modifyFVars,
  ) where


import Control.Applicative
import Data.SafeCopy
import Data.Traversable


-- | A mutable variable (like an MVar) that is stored in a file on disk instead of in memory.
-- Basically an FVar is just a FilePath, which is relative to the set root directory. 
-- (See 'setFVarRoot'.) This should probably be an abstract type.
data FVar a = FVar FilePath
  deriving Show

instance SafeCopy (FVar a) where
    putCopy (FVar f) = contain $ safePut f
    getCopy = contain $ FVar <$> safeGet

-- | Creates a new file containing @a@ on disk and returns the corresponding FVar.
-- Throws an exception if the file already exists.
newFVar :: SafeCopy a => FilePath -> FilePath -> a -> IO (FVar a)
newFVar root file value = nyi

-- | Reads an FVar from Disk.
-- Throws an exception if the file does not exist.
-- Also, throws an exception if the file contains a value
-- of another type, i.e., the caller of this function has to make sure the types match.
openFVar :: FilePath -> IO (FVar a)
openFVar file = nyi

-- > "newFVar" => "createFVar"?  "newFVar" is just as good i guess.

-- | Reads a value from an FVar. Retrieves a non-exclusive lock while doing so. 
-- If you also need write access use 'modifyFVar'.
readFVar :: SafeCopy a => FVar a -> IO a
readFVar (FVar file) = nyi

-- An FVar can always point to a file that doesn't exist anymore?
-- How do we deal with that?
existsFVar :: FVar a -> IO Bool
existsFVar = nyi


-- * synchronized writing access
-- $syncdocs
-- bla bla

class FVars fvars values

instance FVars (FVar a) a
instance (FVars a b, FVars x y) => FVars (a, x) (b, y)
instance (Traversable t) => FVars (t (FVar a)) (t a)

-- | Given some FVars, 'modifyFVars' will atomically retrieve exclusive
-- locks on the corresponding files, will read the values contained,
-- will execute the given operation, will write the resulting values in
-- the corresponding files and will finally release the locks.
--
-- All FVars will be interpreted as relative to the given root
-- directory.
--
-- Locking is done in a central locking file in
-- FVAR_ROOT/.fvarInternals/... Locking of that locking file is done
-- using flock.
--
-- One single call to 'modifyFVars' may block the current thread until
-- another concurrent thread or process releases a lock.
-- A nested call to 'modifyFVars' may block as well, but may also throw
-- a FVarDeadlock exception in case of a deadlock.
modifyFVars :: FVars fvars values =>
    FilePath -> fvars -> (values -> IO (values, result)) -> IO result
modifyFVars = nyi


nyi = error "NYI"
