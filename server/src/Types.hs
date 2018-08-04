{-# LANGUAGE DataKinds, TypeOperators #-}
module Types
    ( Game
    , Board
    , Player
    ) where

import Servant.API

newtype Board = Board [Int]

data Player = White | Black

data Game = Game {
    board :: Board,
    dice :: (Int, Int),
    turn :: Player,
    rng :: Int
}
