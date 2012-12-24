
-- | Initializes the db and generates 50 random values of type Person and writes them to disk.

import Data.Map as Map
import Data.Foldable
import System.Directory
import System.Random
import Control.Applicative

import Data.FVar
import Person


main = do
    index <- initializeDB
    forM_ [1..50] $ const $ do
        p <- randomPerson
        addPersonToDB index p

initializeDB :: IO (FVar PersonIndex)
initializeDB = do
    createDirectory "persons"
    newFVar "persons.index" emptyPersonIndex

addPersonToDB :: FVar PersonIndex -> Person -> IO ()
addPersonToDB pix person = do
    fvar <- newFVarInDirectory "persons" person
    modifyFVar pix $ \ (PersonIndex nextId persons) ->
        return (PersonIndex (succ nextId) (insert nextId fvar persons), ())
    

randomPerson :: IO Person
randomPerson = Person <$>
    mapM (const $ randomRIO ('a', 'z')) [1 .. 10] <*>
    randomRIO (0, 100)
