#!/bin/bash

rm persons* -rf
runhaskell -i.. generateExample.hs
runhaskell -i.. listAllPersons.hs
