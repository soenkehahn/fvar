
import Data.Foldable

import Data.FVar
import Person


main = do
    personIndex <- readFVar =<< openFVar "persons.index"
    print personIndex
    forM_ (persons personIndex) $ \ personFVar ->
        print =<< readFVar personFVar
