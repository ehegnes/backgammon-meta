import Types
import Lib
import Control.Monad

main = do
  print =<< peekPlayer =<< init_player
  print =<< peekPoint =<< init_point
  print =<< peekMaybePoint =<< init_some_point
  print =<< peekMaybePoint =<< init_none_point
  print =<< peekInternalBoard =<< init_internal_board
