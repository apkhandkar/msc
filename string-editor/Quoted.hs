module Quoted
    ( ) where

import SETypes

qini :: String -> SEState
qini _ =
    (SEState{string=" ",cursor=(0)})

testSplitter :: String -> String -> (String, String)
testSplitter
