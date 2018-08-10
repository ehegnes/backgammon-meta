import Types
import Lib

main = do
  print =<< peekPlayer =<< init_player
  print =<< peekPoint =<< init_point
  print =<< peekMaybePoint =<< init_some_point
  print =<< peekMaybePoint =<< init_none_point
