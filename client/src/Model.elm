module Model
    exposing
        ( Model
        , model
        , initial_board
        )

import Types exposing (..)
import GeneratedTypes exposing (..)


-- TODO: use board from API


initial_internal_board =
    InternalBoard
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


initial_board =
    Board initial_internal_board 0 0


type alias Model =
  { board : Board
  }

model : Model
model = { board = initial_board }
