import Lib

main = do
    x <- rust_test_ffi 0
    putStrLn ("Rust returned: " ++ show x)
