module Update exposing
  ( update
  )

import Types exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Http

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewBoard ->
      ( model
      , newBoard
      )
    GetNewBoard result ->
      case result of
        Ok b ->
          ( { model | board = b }
          , Cmd.none
          )
        Err e ->
          ( { model | error = (toString e) }
          , Cmd.none
          )

newBoard : Cmd Msg
newBoard = Http.send GetNewBoard (Http.get "http://localhost:3000/board" jsonDecBoard)
