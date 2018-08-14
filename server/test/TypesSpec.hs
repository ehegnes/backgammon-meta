module TypesSpec where

import Test.Hspec
import Lib
import Types

initial_internal_board = InternalBoard
  [ Just (Point Black 2)
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Just (Point White 5)
  , Nothing
  , Just (Point White 3)
  , Nothing
  , Nothing
  , Nothing
  , Just (Point Black 5)
  , Just (Point White 5)
  , Nothing
  , Nothing
  , Nothing
  , Just (Point Black 3)
  , Nothing
  , Just (Point Black 5)
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Just (Point White 2)
  ]

initial_board = Board initial_internal_board 0 0


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Dice" $ do
    it "can be marshalled" $ do
      dice <- peekDice =<< test_dice
      dice `shouldBe` Dice (2, 6)

  describe "Player" $ do
    it "can be marshalled" $ do
      player <- peekPlayer =<< test_player
      player `shouldBe` Black

  describe "Point" $ do
    it "can be marshalled" $ do
      point <- peekPoint =<< test_point
      point `shouldBe` Point White 5

  describe "MaybePoint" $ do
    it "can be marshalled from `Some(_)`" $ do
      maybePoint <- peekMaybePoint =<< test_some_point
      maybePoint `shouldBe` Just (Point Black 2)
    it "can be marshalled from `None`" $ do
      maybePoint <- peekMaybePoint =<< test_none_point
      maybePoint `shouldBe` Nothing

  describe "InternalBoard" $ do
    it "can be marshalled" $ do
      board <- peekInternalBoard =<< test_internal_board
      board `shouldBe` initial_internal_board

  describe "Board" $ do
    it "can be marshalled" $ do
      board <- peekBoard =<< test_board
      board `shouldBe` initial_board
