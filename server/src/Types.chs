{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

#include "backgammonlogic.h"

import Data.Typeable
import Foreign.C.Types (CInt)
import Foreign.Marshal.Utils (maybePeek)
import Foreign.Marshal.Array
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable
import Control.Monad (liftM)
import GHC.Generics

{#enum define Player {PLAYER_BLACK as Black, PLAYER_WHITE as White}
  deriving (Eq, Ord, Show, Typeable, Generic) #}

instance Storable Player where
  sizeOf    _ = sizeOf    (undefined :: CInt)
  alignment _ = alignment (undefined :: CInt)
  peek p      = liftM cToEnum $ peek (castPtr p :: Ptr CInt)
  poke p v    = poke (castPtr p :: Ptr CInt) (cFromEnum v)

cToEnum :: (Integral i, Enum e) => i -> e
cToEnum  = toEnum . fromIntegral

cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum = fromIntegral . fromEnum

peekPlayer :: Ptr Player -> IO Player
peekPlayer = peek

type Die = {#type Die #}

data Dice = Dice (Die, Die)
  deriving (Eq, Show, Typeable)

instance Storable Dice where
  sizeOf _ = {#sizeof RustDice #}
  alignment _ = {#alignof RustDice #}
  peek p = do
    d1 <- {#get RustDice->d1 #} p
    d2 <- {#get RustDice->d2 #} p
    return $ Dice (fromIntegral d1 :: Die, fromIntegral d2 :: Die)
  poke = undefined

peekDice :: Ptr Dice -> IO Dice
peekDice = peek

data Point = Point
  { owner :: Player
  , count :: Int
  } deriving (Eq, Show, Typeable, Generic)

instance Storable Point where
  sizeOf _ = {#sizeof RustPoint #}
  alignment _ = {#alignof RustPoint #}
  peek p = Point
    <$> liftM cToEnum      ({#get RustPoint->owner #} p)
    <*> liftM fromIntegral ({#get RustPoint->count #} p)
  poke p x = do
    {#set RustPoint.owner #} p (cFromEnum $ owner x)
    {#set RustPoint.count #} p (fromIntegral $ count x)

peekPoint :: Ptr Point -> IO Point
peekPoint p = do
  owner <- {#get RustPoint->owner #} p
  count <- {#get RustPoint->count #} p
  return $ Point (cToEnum owner) (fromIntegral count)

{#pointer *RustPoint as MaybePoint -> Point #}
peekMaybePoint :: MaybePoint -> IO (Maybe Point)
peekMaybePoint = maybePeek peekPoint

--instance Storable InternalBoard where
--  sizeOf _ = {#sizeof RustInternalBoard #}
--  alignment _ = {#alignof RustInternalBoard #}
--  peek p = do
--    maybePoints <- peekArray 24 $ castPtr p
--    points <- sequence $ fmap peekMaybePoint maybePoints
--    return $ InternalBoard points
--  poke p = undefined

data Board = Board
  { board :: [Maybe Point]
  , barBlack :: Int
  , barWhite :: Int
  } deriving (Eq, Show, Typeable, Generic)

instance Storable Board where
  sizeOf _ = {#sizeof RustBoard #}
  alignment _ = {#alignof RustBoard #}
  peek p = do
    maybePoints <- peekArray 24 $ castPtr p
    board <- sequence (peekMaybePoint <$> maybePoints)
    barBlack <- fromIntegral <$> {#get RustBoard->bar_black #} p
    barWhite <- fromIntegral <$> {#get RustBoard->bar_white #} p
    return $ Board board barBlack barWhite
  poke = undefined

peekBoard :: Ptr Board -> IO Board
peekBoard = peek . castPtr
