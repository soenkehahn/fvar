-- -------------------------------------------------------------------------------
-- WARNING: This is not a fully working implementation.
-- The given implementations are just there because I want to try some things out.
-- Very crucial features (like file locking) are deliberately omitted.
-- -------------------------------------------------------------------------------

{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}


module FVar where

import Control.Applicative
import Data.ByteString as SBS (readFile, writeFile)
import Data.SafeCopy
import Data.Serialize.Get
import Data.Serialize.Put
import System.Directory
import System.FilePath


-- | Sets the root directory. All paths are relative to that root. If no 
-- root is set when the first file system access happens, or if the root 
-- does not exist as a directory, or if access permissions are missing,
-- exceptions are thrown.
setFVarRoot :: FilePath -> IO ()
setFVarRoot = error "setFVarRoot"

-- | Returns the set root. 
getFVarRoot :: IO FilePath
getFVarRoot = error "getFVarRoot"

-- | A mutable variable (like an MVar) that is stored in a file on disk instead of in memory.
-- Basically an FVar is just a FilePath, which is relative to the set root directory. 
-- (See 'setFVarRoot'.) This should probably be an abstract type.
data FVar a = FVar FilePath
  deriving Show

instance SafeCopy (FVar a) where
    putCopy (FVar f) = contain $ safePut f
    getCopy = contain $ FVar <$> safeGet

-- | Retrieves a write lock and performs the given action. Writes the new @a@ to disk.
modifyFVar :: SafeCopy a => FVar a -> (a -> IO (a, b)) -> IO b
modifyFVar fvar@(FVar file) action = do
    x <- readFVar fvar
    (x', output) <-  action x
    SBS.writeFile file (runPut (safePut x'))
    return output
    
-- | Reads a value from an FVar. Retrieves a non-exclusive lock while doing so. 
-- If you also need write access use 'modifyFVar'.
readFVar :: SafeCopy a => FVar a -> IO a
readFVar (FVar file) = do
    c <- SBS.readFile file
    return $ case runGet safeGet c of
        Right x -> x
        Left err -> error ("error when parsing file: " ++ err)

-- | Creates a new file containing @a@ on disk and returns the corresponding FVar.
-- Throws an exception if the file already exists.
newFVar :: SafeCopy a => FilePath -> a -> IO (FVar a)
newFVar file value = do
    SBS.writeFile file (runPut (safePut value))
    return $ FVar file

-- | Reads an FVar from Disk.
-- Throws an exception if the file does not exist.
-- Also, throws an exception if the file contains a value
-- of another type, i.e., the caller of this function has to make sure the types match.
openFVar :: FilePath -> IO (FVar a)
openFVar file = return (FVar file)

-- > "newFVar" => "createFVar"?  "newFVar" is just as good i guess.


-- An FVar can always point to a file that doesn't exist anymore?
-- How do we deal with that?
existsFVar :: FVar a -> IO Bool
existsFVar = error "exists"


-- convenience

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
    files <- getDirectoryContents directory
    let newFile = head $ filter (\ f -> not (f `elem` files)) allPossibleFileNames
        allPossibleFileNames = map show [0 ..]
    newFVar (directory </> newFile) value

-- > hm...  "newFVarInDirectory" seems conceptually dirty to me.  what
-- about this instead?
-- >> I hope, the new comment clarifies this. Does it still seem dirty?
-- >> {set,get}FVarRoot are still a good idea.
