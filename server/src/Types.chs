{-# LANGUAGE DeriveDataTypeable #-}

module Types where

#include "backgammonlogic.h"

import Data.Typeable
import Foreign.C.Types (CInt)
import Foreign.Marshal.Utils (maybePeek)
import Foreign.Marshal.Array
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable
import Control.Monad (liftM)

{#enum define Player {PLAYER_BLACK as Black, PLAYER_WHITE as White} deriving (Eq, Ord, Show) #}

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

newtype Dice = Dice {#type RustDice #}

data Point = Point
  { owner :: Player
  , count :: Int
  } deriving (Eq, Show, Typeable)

instance Storable Point where
  sizeOf _ = {#sizeof RustPoint #}
  alignment _ = 4
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

data InternalBoard = InternalBoard [Maybe Point]
  deriving (Eq, Show, Typeable)

instance Storable InternalBoard where
  sizeOf _ = {#sizeof RustInternalBoard #}
  alignment _ = {#alignof RustInternalBoard #}
  peek p = do
    maybePoints <- peekArray 24 $ castPtr p
    points <- sequence $ fmap peekMaybePoint maybePoints
    return $ InternalBoard points
  poke p = undefined

peekInternalBoard :: Ptr InternalBoard -> IO InternalBoard
peekInternalBoard = peek . castPtr
