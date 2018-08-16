{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Lib
import Types

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Network.Wai.Handler.Warp (run)
import Servant

type BoardAPI = "board" :> Get '[JSON] Board

server :: Server BoardAPI
server = liftIO $ peekBoard =<< test_board

boardAPI :: Proxy BoardAPI
boardAPI = Proxy

app :: Application
app = serve boardAPI server

main :: IO ()
main = do
  print "Try `curl localhost:3000/board` in another terminal"
  run  3000 app
