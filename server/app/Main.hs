{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C.Types

foreign import ccall unsafe "rust_test_ffi" rust_test_ffi :: CInt -> IO CInt

{-
 - Write the function to grab the board in here.
 - }

main = do
    x <- rust_test_ffi 0
    putStrLn ("Rust returned: " ++ show x)
