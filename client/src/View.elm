module View exposing
  ( view
  )

import Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Msg exposing (..)
import Model exposing (..)

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick NewBoard ] [ text "New Board" ]
    , div [] [ text (toString model.board) ]
    , div [ errorTextStyle ] [ text ("Error: " ++ (toString model.error)) ]
    ]

errorTextStyle = style
  [ ("color", "red")
  ]
