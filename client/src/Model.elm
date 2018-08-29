module Model exposing
  ( ..
  )

import Msg exposing (..)
import Types exposing (..)

-- TODO: use board from API

init_board = Board
  (InternalBoard
    [ Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    , Nothing
    ])
  0
  0

type alias Model =
  { board : Board
  , error : String
  }

init : (Model, Cmd Msg)
init =
  ( { board = init_board
    , error = "None"
    }
  , Cmd.none
  )
