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


-- | A mutable variable (like an MVar) that is stored in a file on disk instead of in memory.
-- Basically an FVar is just a relative FilePath.
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
    

-- | Reads a value from an FVar. If you also need write access use 'modifyFVar'.
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
-- of another type. The caller of this function has to make sure the types match.
getFromDisk :: FilePath -> IO (FVar a)
getFromDisk file = return (FVar file)

-- An FVar can always point to a file that doesn't exist anymore?
-- How do we deal with that?
exists :: FVar a -> IO Bool
exists = error "exists"


-- convenience

newFVarInDirectory :: SafeCopy a => FilePath -> a -> IO (FVar a)
newFVarInDirectory directory value = do
    files <- getDirectoryContents directory
    let newFile = head $ filter (\ f -> not (f `elem` files)) allPossibleFileNames
        allPossibleFileNames = map show [0 ..]
    newFVar (directory </> newFile) value
