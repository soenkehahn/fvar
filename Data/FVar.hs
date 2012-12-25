{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}


module Data.FVar (
    FVar,
    module Data.FVar,
  ) where


import Data.FVar.Core (FVar, nyi)
import qualified Data.FVar.Core as Core


-- | Runs the given action in a transaction on an FVar-store.
-- The location of the FVar-store is given by the filepath.
-- Transactions can be nested.
-- This is a simple wrapper around 'Data.FVar.Core.withTransaction'.
-- It uses the ThreadId to manage transactions. So one transaction is
-- bound to one thread.
withTransaction :: FilePath -> IO a -> IO a
withTransaction = nyi

newFVar :: FilePath -> a -> IO (FVar a)
newFVar = nyi

openFVar :: FilePath -> IO (FVar a)
openFVar = nyi

readFVar :: FVar a -> IO a
readFVar = nyi

-- | Stores a new value, destroying the previous one.
writeFVar :: FVar a -> a -> IO ()
writeFVar = nyi


-- * convenience

modifyFVar :: FVar a -> (a -> IO (a, b)) -> IO b
modifyFVar fvar action = error "NYI"

newFVarInDirectory :: FilePath -> a -> IO (FVar a)
newFVarInDirectory directory value = nyi
