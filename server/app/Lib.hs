module Lib where

import Foreign.C.Types
import Foreign.Ptr
import Types
import Data.Word
import Control.Monad (liftM)

foreign import ccall unsafe init_player :: IO (Ptr Player)
foreign import ccall unsafe init_point :: IO (Ptr Point)
foreign import ccall unsafe init_some_point :: IO MaybePoint
foreign import ccall unsafe init_none_point :: IO MaybePoint
foreign import ccall unsafe init_internal_board :: IO (Ptr InternalBoard)

{-getInternalBoard :: IO (Ptr Word8) -> IO (Ptr Int)-}
{-getInternalBoard  = liftM castPtr-}
