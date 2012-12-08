
module Person where


import Control.Applicative ((<$>), (<*>))
import Data.SafeCopy
import Data.Map

import FVar


data PersonIndex = PersonIndex {
    nextId :: Int,
    persons :: Map Int (FVar Person)
  }
    deriving Show

emptyPersonIndex = PersonIndex 0 empty

instance SafeCopy PersonIndex where
    putCopy (PersonIndex nextId persons) = contain $ do
        safePut nextId
        safePut persons
    getCopy = contain $ PersonIndex <$> safeGet <*> safeGet

data Person = Person {
    name :: String,
    age :: Int
  }
    deriving Show

instance SafeCopy Person where
    putCopy (Person name age) = contain $ do
        safePut name
        safePut age
    getCopy = contain $ Person <$> safeGet <*> safeGet
