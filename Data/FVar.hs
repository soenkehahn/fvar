-- -------------------------------------------------------------------------------
-- WARNING: This is not a fully working implementation.
-- The given implementations are just there because I want to try some things out.
-- Very crucial features (like file locking) are deliberately omitted.
-- -------------------------------------------------------------------------------

{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}


module Data.FVar (
    FVar,
    module Data.FVar,
  ) where

import Data.SafeCopy
import System.Directory
import System.FilePath

import Data.FVar.Core (FVar, FVars)
import qualified Data.FVar.Core as Core

-- | Sets the root directory. All paths are relative to that root. If no 
-- root is set when the first file system access happens, or if the root 
-- does not exist as a directory, or if access permissions are missing,
-- exceptions are thrown.
setFVarRoot :: FilePath -> IO ()
setFVarRoot = error "setFVarRoot"

-- | Returns the set root. 
getFVarRoot :: IO FilePath
getFVarRoot = error "getFVarRoot"

newFVar :: FilePath -> a -> IO (FVar a)
newFVar = nyi

openFVar :: FilePath -> IO (FVar a)
openFVar = nyi

readFVar :: FVar a -> IO a
readFVar = nyi

-- | Retrieves a write lock and performs the given action. Writes the
-- new @a@ to disk.
modifyFVar :: SafeCopy a => FVar a -> (a -> IO (a, b)) -> IO b
modifyFVar fvar action = error "NYI"

modifyFVars :: FVars fvars values =>
    fvars -> (values -> IO (values, result)) -> IO result
modifyFVars = error "NYI"


-- * convenience

-- | Stores a new value, destroying the previous one.
writeFVar :: SafeCopy a => FVar a -> a -> IO ()
writeFVar fvar value = modifyFVar fvar (const $ return (value, ()))

-- | If you want to create a new FVar, but you don't care about its filename, 
-- you can use 'newFVarInDirectory'. It creates a new filename that doesn't 
-- clash with any file in the given directory and calls 'newFVar'.
-- The given directory should be relative to the set root directory. 
-- (See 'setFVarRoot'.)
newFVarInDirectory :: SafeCopy a => FilePath -> a -> IO (FVar a)
newFVarInDirectory directory value = do
    -- There is a race condition, where this still clashes.
    files <- getDirectoryContents directory
    let newFile = head $ filter (\ f -> not (f `elem` files)) allPossibleFileNames
        allPossibleFileNames = map show [0 ..]
    newFVar (directory </> newFile) value


nyi = error "NYI"
