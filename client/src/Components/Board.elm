module Components.Board exposing
  ( view
  )

import Html exposing (..)
import Html.Attributes exposing (..)

import Types exposing (..)
import Msg exposing (Msg)
import Model exposing (..)
import List exposing (length, range, map2)

view : Model -> Html Msg
view model =
  div
    []
    [ viewBoard model.board
    ]

pointToColor { owner } =
  case owner of
    Black -> "grey"
    White -> "lightblue"

zip = List.map2 (,)

viewBoard : Board -> Html msg
viewBoard { board } =
  let
      topHalf = List.take 12 board
      bottomHalf = List.take 12 (List.drop 12 board)
  in
      div
        []
        [ div [style [("height", "64px")]] (viewHalf topHalf)
        , div [] (viewHalf bottomHalf)
        ]


viewHalf : List (Maybe Point) -> List (Html msg)
viewHalf b =
  let
      board = zip (range 0 (length b)) b
  in
    List.map (\(i, maybePoint) ->
      case maybePoint of
        Just point ->
          -- TODO: map such that each chequer is represented
          div
            [ style
              [ ("borderRadius", "50%")
              , ("backgroundColor", (pointToColor point))
              , ("width", "32px")
              , ("height", "32px")
              , ("marginLeft", (toString (i*32) ++ "px"))
              , ("position", "absolute")
              ]
            ]
            []
        Nothing ->
          div [] []
      ) board
