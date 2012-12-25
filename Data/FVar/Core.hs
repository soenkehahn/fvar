{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}

-- | This is the core of the FVar library. Everything else should be
-- implemented in terms of this module. This is also the module that
-- needs to be tested extensively.
--
-- The id is needed to do proper deadlock detection. The given id should
-- be unique for every thread that calls the functions.

module Data.FVar.Core (
    -- * construction and reading
    TransactionId,
    withTransaction,
    FVar(..),
    newFVar,
    openFVar,
    existsFVar,

    readFVar,
    writeFVar,

    nyi,
  ) where


import Data.SafeCopy


data TransactionId = TId

-- | Performs the given action in a transaction on an FVar-store.
-- The location of the FVar-store is given by the filepath.
-- Transactions can be nested.
withTransaction :: FilePath -> (TransactionId -> IO a) -> IO a
withTransaction root = nyi

-- | A mutable variable (like an MVar) that is stored in a file on disk
-- instead of in memory. Basically an FVar is just a FilePath, which is
-- relative to the set root directory. (See 'setFVarRoot'.)
data FVar a where
    FVar :: SafeCopy a => FilePath -> FVar a

instance Show (FVar a) where
    show = nyi

instance SafeCopy (FVar a) where
    putCopy = nyi
    getCopy = nyi

-- | Creates a new file containing @a@ on disk and returns the
-- corresponding FVar. Throws an exception if the file already exists.
newFVar :: TransactionId -> FilePath -> a -> IO (FVar a)
newFVar id file value = nyi

-- | Reads an FVar from Disk.
-- Throws an exception if the file does not exist.
-- Also, throws an exception if the file contains a value of another
-- type, i.e., the caller of this function has to make sure the types
-- match.
openFVar :: TransactionId -> FilePath -> IO (FVar a)
openFVar id file = nyi

-- | Deletes an FVar from the store. Don't use the FVar after that.
-- (Or use 'existsFVar' to verify the files existence.)
deleteFVar :: TransactionId -> FVar a -> IO ()
deleteFVar = nyi

-- Returns whether the file of an FVar still exists.
existsFVar :: TransactionId -> FVar a -> IO Bool
existsFVar = nyi

-- | Reads a value from an FVar.
readFVar :: TransactionId -> FVar a -> IO a
readFVar id (FVar file) = nyi

-- | Writes a value to a given file.
writeFVar :: TransactionId -> FVar a -> a -> IO ()
writeFVar = nyi


-- development

nyi = error "NYI"
