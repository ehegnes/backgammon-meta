{-# LANGUAGE TemplateHaskell #-}

module Main where

import Types
import Elm.Derive
import Elm.Module
import Data.Proxy

deriveBoth defaultOptions ''Board
deriveBoth defaultOptions ''Player
deriveBoth defaultOptions ''Point
deriveBoth defaultOptions ''Submove
deriveBoth defaultOptions ''Move

main :: IO ()
main =
    writeFile "../client/src/Types.elm"
    $ makeElmModule "Types"
    [ DefineElm (Proxy :: Proxy Player)
    , DefineElm (Proxy :: Proxy Point)
    , DefineElm (Proxy :: Proxy Board)
    , DefineElm (Proxy :: Proxy Submove)
    , DefineElm (Proxy :: Proxy Move)
    ]
