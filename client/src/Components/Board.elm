module Components.Board exposing
  ( view
  )

import Html exposing (..)
import Html.Attributes exposing (..)

import Types exposing (..)
import Msg exposing (Msg)
import Model exposing (..)
import List exposing (length, range, map2)


type alias MyList =
  { l: List Int
  }

initList : MyList
initList = MyList [1, 2, 3, 4]

view : Model -> Html Msg
view model =
  div []
    [ div [] (viewChequers model.board.board)
    ]

pointToColor { owner } =
  case owner of
    Black -> "grey"
    White -> "lightblue"

zip = List.map2 (,)

-- FIXME: This should take a `Board` instead of an `InternalBoard` and be
--        renamed
viewChequers : InternalBoard -> List (Html msg)
viewChequers (InternalBoard b) =
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
