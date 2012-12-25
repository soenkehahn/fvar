
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Property
import System.IO.Temp

import Data.FVar.Core


main = do
    quickCheck newAndRead
    quickCheck writeAndRead

data Data = Data Int
  deriving (Eq, Show)

instance Arbitrary Data where
    arbitrary = Data <$> arbitrary
    shrink (Data i) = map Data $ shrink i

newAndRead :: Data -> Property
newAndRead d =
    morallyDubiousIOProperty $
    withSystemTempDirectory "fvar-tests" $ \ root ->
    withTransaction root $ \ t -> do
        fvar <- newFVar t "file" d
        r <- readFVar t fvar
        return (d == r)

writeAndRead :: Data -> Data -> Property
writeAndRead a b =
    morallyDubiousIOProperty $
    withSystemTempDirectory "fvar-tests" $ \ root ->
    withTransaction root $ \ t -> do
        fvar <- newFVar t "file" a
        writeFVar t fvar b
        r <- readFVar t fvar
        return (b == r)
