module Components.Board exposing
  ( view
  )

import Html exposing (..)
import Html.Attributes exposing (..)

import Types exposing (..)
import Msg exposing (Msg)
import Model exposing (..)
import List exposing (length, range, map2, reverse, take, drop)

view : Model -> Html Msg
view model =
  div
    []
    [ viewBoard model.board
    ]

pointToColor { owner } =
  case owner of
    Black -> "#999"
    White -> "lightblue"

zip = List.map2 (,)

viewBoard : Board -> Html msg
viewBoard { board } =
  let
    topHalf = List.take 12 board
    bottomHalf = reverse (take 12 (drop 12 board))
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
              [ ("position", "absolute")
              , ("marginLeft", (toString (i*32) ++ "px"))
              , ("backgroundColor", (pointToColor point))
              , ("borderRadius", "50%")
              , ("width", "32px")
              , ("height", "32px")
              , ("lineHeight", "32px")
              , ("textAlign", "center")
              ]
            ]
            [ text (toString point.count) ]
        Nothing ->
          div
            [ style
              [ ("position", "absolute")
              , ("marginLeft", (toString (i*32) ++ "px"))
              , ("borderRadius", "50%")
              , ("border", "1px solid black")
              , ("width", "32px")
              , ("height", "32px")
              ]
            ]
            []
      ) board
