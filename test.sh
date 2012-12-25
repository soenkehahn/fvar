#!/bin/bash

runhaskell -XCPP -Dnyi="error (__FILE__ ++ \":\" ++ show __LINE__ ++ \":0\")" Test/Main.hs
