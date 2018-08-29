module Msg exposing (..)

import Http
import Types exposing (..)

type Msg = NewBoard
         | GetNewBoard (Result Http.Error Board)
