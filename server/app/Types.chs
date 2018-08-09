module Types where

#include "backgammonlogic.h"

import Foreign.C.String
import Control.Applicative
import Foreign.C.Types

{#enum define Player {PLAYER_BLACK as Black, PLAYER_WHITE as White} deriving (Eq, Ord) #}

type Die = {#type Die#}

newtype Dice = Dice {#type RustDice#}

getD1 = {#get RustDice->d1#}
getD2 = {#get RustDice->d2#}

getDice  = (getD1, getD2)
