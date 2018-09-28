{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

#include "backgammonlogic.h"

import Data.Typeable
import Foreign.C.Types (CInt)
import Foreign.Marshal.Utils (maybePeek)
import Foreign.Marshal.Array
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable
import Control.Monad (liftM, join)
import GHC.Generics

{#enum define Player
  { PLAYER_BLACK as Black
  , PLAYER_WHITE as White
  } deriving (Eq, Ord, Show, Typeable, Generic) #}

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
  deriving (Eq, Show, Typeable, Generic)

instance Storable Dice where
  sizeOf _ = {#sizeof RustDice #}
  alignment _ = {#alignof RustDice #}
  peek p = do
    d1 <- {#get RustDice.d1 #} p
    d2 <- {#get RustDice.d2 #} p
    return $ Dice (d1, d2)
  poke = undefined

peekDice :: Ptr Dice -> IO Dice
peekDice = peek . castPtr

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
peekPoint = peek . castPtr

{#pointer *RustPoint as MaybePoint -> Point #}
peekMaybePoint :: MaybePoint -> IO (Maybe Point)
peekMaybePoint = maybePeek peekPoint

data Board = Board
  { _board :: [Maybe Point]
  , barBlack :: Int
  , barWhite :: Int
  } deriving (Eq, Show, Typeable, Generic)

instance Storable Board where
  sizeOf _ = {#sizeof RustBoard #}
  alignment _ = {#alignof RustBoard #}
  peek p = do
    maybePoints <- peekArray 24 $ castPtr p
    _board <- sequence (peekMaybePoint <$> maybePoints)
    barBlack <- fromIntegral <$> {#get RustBoard->bar_black #} p
    barWhite <- fromIntegral <$> {#get RustBoard->bar_white #} p
    return $ Board _board barBlack barWhite
  poke = undefined

peekBoard :: Ptr Board -> IO Board
peekBoard = peek . castPtr

{#enum define SubmoveTag
  { SUBMOVE_BEAR_OFF as TagBearOff
  , SUBMOVE_ENTER as TagEnter
  , SUBMOVE_MOVE as TagMove
  } deriving (Eq, Ord, Show, Typeable, Generic) #}

data Submove
  = SubmoveBearOff Int
  | SubmoveEnter Int
  | SubmoveMove Int Int
  deriving (Eq, Show, Typeable, Generic)

instance Storable Submove where
  sizeOf _ = {#sizeof RustSubmovePayload #}
  alignment _ = {#alignof RustSubmovePayload #}
  peek p = do
    tag <- liftM cToEnum $ {#get RustSubmove->tag #} p :: IO SubmoveTag
    payload <- {#get RustSubmove->payload #} p
    case tag of
      TagBearOff -> do
        from <- fromIntegral <$> {#get RustSubmoveBearOff.from #} payload
        return $ SubmoveBearOff from
      TagEnter -> do
        to <- fromIntegral <$> {#get RustSubmoveEnter.to #} payload
        return $ SubmoveEnter to
      TagMove -> do
        from <- fromIntegral <$> {#get RustSubmoveMove.from #} payload
        to <- fromIntegral <$> {#get RustSubmoveMove.to #} payload
        return $ SubmoveMove from to
  poke = undefined

peekSubmove :: Ptr Submove -> IO Submove
peekSubmove = peek . castPtr

data Move = Move [Submove]
  deriving (Eq, Show, Typeable, Generic)

instance Storable Move where
  sizeOf _ = {#sizeof RustMove #}
  alignment _ = {#alignof RustMove #}
  peek p = do
    submoves <- peekArray0 nullPtr $ castPtr p :: IO [Ptr Submove]
    moves <- sequence (peekSubmove <$> submoves)
    return $ Move moves
  poke = undefined

peekMove :: Ptr Move -> IO Move
peekMove = peek . castPtr

data Game = Game
  { board :: Board
  , dice :: Dice
  , turn :: Player
} deriving (Eq, Show, Typeable, Generic)

instance Storable Game where
  sizeOf _ = {#sizeof RustGame #}
  alignment _ = {#alignof RustGame #}
  peek p = do
    board <- peekBoard . castPtr =<< {#get RustGame->board #} p
    dice <- peekDice . castPtr =<< {#get RustGame->dice #} p
    turn <- peekPlayer . castPtr =<< {#get RustGame->turn #} p
    return $ Game board dice turn
  poke = undefined

peekGame :: Ptr Game -> IO Game
peekGame = peek . castPtr
