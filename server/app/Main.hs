{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Lib
import Types

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant

deriving instance ToJSON Board
deriving instance ToJSON Player
deriving instance ToJSON Point
deriving instance ToJSON Submove
deriving instance ToJSON Move
deriving instance ToJSON Dice
deriving instance ToJSON Game

type GameAPI = Get '[JSON] Game
type MoveAPI = Get '[JSON] Move

type API = "game" :> GameAPI
      :<|> "move" :> MoveAPI

gameServer :: Server GameAPI
gameServer = liftIO $ peekGame =<< test_game

moveServer :: Server MoveAPI
moveServer = liftIO $ peekMove =<< test_move

api :: Proxy API
api = Proxy

server :: Server API
server = gameServer
    :<|> moveServer

app :: Application
app = serve api server

main :: IO ()
main = do
  run 3000 $ simpleCors $ logStdoutDev app
