module Lib where

import Foreign.C.Types

foreign import ccall unsafe "rust_test_ffi" rust_test_ffi :: CInt -> IO CInt
