module Lib where

import Foreign.C.Types

foreign import ccall unsafe "rust_test_ffi" rust_test_ffi :: CInt -> IO CInt

{-
 - --- Fix the types and try to print in `Main`
 -foreign import ccall unsafe "init_game" init_game :: CInt -> IO CInt
 -}
