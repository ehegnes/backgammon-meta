module Lib where

import Foreign.Ptr (Ptr)
import Types

foreign import ccall unsafe test_dice :: IO (Ptr Dice)
foreign import ccall unsafe test_player :: IO (Ptr Player)
foreign import ccall unsafe test_point :: IO (Ptr Point)
foreign import ccall unsafe test_some_point :: IO MaybePoint
foreign import ccall unsafe test_none_point :: IO MaybePoint
foreign import ccall unsafe test_board :: IO (Ptr Board)
