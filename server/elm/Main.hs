{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Data.Proxy
import Types
import Elm
import Servant.Elm
import Servant.API

instance ElmType Board
instance ElmType InternalBoard
instance ElmType Player
instance ElmType Point

type BoardAPI = "board" :> Get '[JSON] Board

spec :: Spec
spec = Spec ["GeneratedTypes"]
  (defElmImports
  : "import Types exposing (..)"
  : toElmTypeSource    (Proxy :: Proxy Board)
  : toElmDecoderSource (Proxy :: Proxy Board)
  : toElmTypeSource    (Proxy :: Proxy InternalBoard)
  : toElmDecoderSource (Proxy :: Proxy InternalBoard)
  {-: toElmTypeSource    (Proxy :: Proxy Player)-}
  {-: toElmDecoderSource (Proxy :: Proxy Player)-}
  : toElmTypeSource    (Proxy :: Proxy Point)
  : toElmDecoderSource (Proxy :: Proxy Point)
  : generateElmForAPI  (Proxy :: Proxy BoardAPI)
  )

main :: IO ()
main = specsToDir [spec] "../client/src"
