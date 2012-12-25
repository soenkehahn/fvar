
module Data.FVar.Convenience where


import Data.FVar.Core


modifyFVar :: TransactionId -> FVar a -> (a -> IO (a, b)) -> IO b
modifyFVar = nyi

-- | If you want to create a new FVar, but you don't care about its filename, 
-- you can use 'newFVarInDirectory'. It creates a new filename that doesn't 
-- clash with any file in the given directory and calls 'newFVar'.
-- The given directory should be relative to the set root directory. 
-- (See 'setFVarRoot'.)
newFVarInDirectory :: TransactionId -> FilePath -> a -> IO (FVar a)
newFVarInDirectory id directory value = nyi
