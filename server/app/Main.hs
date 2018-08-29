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
deriving instance ToJSON InternalBoard
deriving instance ToJSON Player
deriving instance ToJSON Point

type BoardAPI = "board" :> Get '[JSON] Board

server :: Server BoardAPI
server = liftIO $ peekBoard =<< test_board

boardAPI :: Proxy BoardAPI
boardAPI = Proxy

app :: Application
app = serve boardAPI server

main :: IO ()
main = do
  run 3000 $ simpleCors $ logStdoutDev app
