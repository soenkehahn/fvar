{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import System.IO.Temp
import Test.QuickCheck
import Test.QuickCheck.Property

import Data.FVar.Core


main = do
    quickCheck concurrent

data Data = Data Int
  deriving (Eq, Show)

instance Arbitrary Data where
    arbitrary = Data <$> arbitrary
    shrink (Data i) = map Data $ shrink i

data ConcurrentAction = ConcurrentAction {
    name :: String,
    action :: Property
  }

instance Show ConcurrentAction where
    show = name

data TestAction = TestAction {
    testAction :: ConcurrentAction,
    startTime :: Int
  }
    deriving (Show)

instance Arbitrary TestAction where
    arbitrary = do
        action <- arbitrary
        startTime <- choose (0, 1)
        return $ TestAction action startTime
    shrink TestAction{..} =
        TestAction <$> shrink testAction <*> shrink startTime

concurrent :: [TestAction] -> Property
concurrent actions = testRaceConditions $ morallyDubiousIOProperty $ do
    threads <- mapM fork actions
    results <- mapM waitForResult threads
    return $ conjoin results
  where
    fork :: TestAction -> IO (MVar Property)
    fork a = do
        mvar <- newEmptyMVar
        forkIO $ do
            threadDelay (startTime a)
            putMVar mvar (action $ testAction a)
                `onException` (putMVar mvar (property False))
        return mvar
    waitForResult :: MVar Property -> IO Property
    waitForResult mvar = do
        takeMVar mvar

testRaceConditions :: Property -> Property
testRaceConditions p = foldl (.&&.) (property True) (replicate 1000 p)


-- * concurrent actions

instance Arbitrary ConcurrentAction where
    arbitrary = elements $
        newAndRead :
        writeAndRead :
        []

transactionTest :: (TransactionId -> IO Property) -> Property
transactionTest action =
    morallyDubiousIOProperty $
    withSystemTempDirectory "fvar-tests" $ \ root ->
    withTransaction root action

newAndRead :: ConcurrentAction
newAndRead = ConcurrentAction "newAndRead" $
    property $ \ (d :: Data) ->
    transactionTest $ \ t -> do
        fvar <- newFVar t "file" d
        r <- readFVar t fvar
        return $ property (d == r)

writeAndRead :: ConcurrentAction
writeAndRead = ConcurrentAction "writeAndRead" $
    property $ \ (a :: Data) (b :: Data) ->
    morallyDubiousIOProperty $
    withSystemTempDirectory "fvar-tests" $ \ root ->
    withTransaction root $ \ t -> do
        fvar <- newFVar t "file" a
        writeFVar t fvar b
        r <- readFVar t fvar
        return (b == r)
