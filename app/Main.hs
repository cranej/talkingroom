{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
    get "/" $ do
        html "Hello World!"
