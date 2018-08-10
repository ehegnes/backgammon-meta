module Lib where

import Foreign.C.Types
import Foreign.Ptr
import Types

foreign import ccall unsafe init_player :: IO (Ptr Player)
foreign import ccall unsafe init_point :: IO (Ptr Point)
foreign import ccall unsafe init_some_point :: IO MaybePoint
foreign import ccall unsafe init_none_point :: IO MaybePoint
