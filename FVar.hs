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

-- > why relative?  where does it root in?  does it have to be relative, or can the user decide whether it is relative or not?
-- > my guess: "... is just a FilePath.  Where it points to, whether it is relative or absolute etc. is up to the caller."
-- > or: "... is just a relative FilePath.  The root is ..."

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
    
-- | Store a new value, destroying the previous one.
writeFVar :: SafeCopy a => FVar a -> a -> IO ()
writeFVar fvar value = modifyFVar fvar (const $ return (value, ()))


-- | Reads a value from an FVar. If you also need write access use 'modifyFVar'.

-- > no locking here?  i think you'll also have to lock for read access, or you may end up
-- > reading half of the old object and half of the new.

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
getFromDisk :: FilePath -> IO (FVar a)
getFromDisk file = return (FVar file)

-- > "newFVar" => "createFVar"?  "getFromDisk" => "loadFVar", or
-- > "openFVar"?  "newFVar" is just as good i guess.  I like "openFVar",
-- > because it doesn't really read the value into ram, but just makes
-- > it available for read and write access.



-- An FVar can always point to a file that doesn't exist anymore?
-- How do we deal with that?
exists :: FVar a -> IO Bool
exists = error "exists"

-- > "existsFVar"?  (i'm not very passionate about this either.)


-- convenience

newFVarInDirectory :: SafeCopy a => FilePath -> a -> IO (FVar a)
newFVarInDirectory directory value = do
    files <- getDirectoryContents directory
    let newFile = head $ filter (\ f -> not (f `elem` files)) allPossibleFileNames
        allPossibleFileNames = map show [0 ..]
    newFVar (directory </> newFile) value

-- > hm...  "newFVarInDirectory" seems conceptually dirty to me.  what
-- about this instead?

setFVarRoot :: FilePath -> IO ()

getFVarRoot :: IO FilePath

-- > and then all paths are relative to that root.  if no root is set
-- when the first file system access happens, or if the root does not
-- exist as a directory, or if access permissions are missing,
-- exceptions are thrown.
