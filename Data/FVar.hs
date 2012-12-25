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


import Data.FVar.Core (FVar, nyi)
import qualified Data.FVar.Core as Core


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
