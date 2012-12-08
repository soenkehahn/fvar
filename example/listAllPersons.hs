
import Data.Foldable

import FVar
import Person


main = do
    personIndex <- readFVar =<< getFromDisk "persons.index"
    print personIndex
    forM_ (persons personIndex) $ \ personFVar ->
        print =<< readFVar personFVar
