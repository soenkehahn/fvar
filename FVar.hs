{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}


import Control.Applicative
import Data.ByteString as SBS
import Data.SafeCopy
import Data.Serialize.Get
import Data.Serialize.Put


e = error "nyi"


data FVar a = FVar FilePath

-- | Retrieves a write lock and performs the given action. Writes the new @a@ to disk.
modifyFVar :: FVar a -> (a -> IO (a, b)) -> IO b
modifyFVar = e

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
getFromDisk file = e

-- An FVar can always point to a file that doesn't exist anymore?
-- How do we deal with that?
exists :: FVar a -> IO Bool
exists = e


-- flock style file locking could be used.
-- How can you lock several files atomically while preventing deadlocks?

-- Do we want some kind of type representation in the files?
--      Would be necessary to implement 'getFromDisk' the way it is documented.
