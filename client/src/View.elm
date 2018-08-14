module View exposing (translate, view)

import Types exposing (..)
import List exposing (map)
import Html exposing (Html, div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


view : Model -> Html Msg
view model =
    viewBoard model


viewBoard : Model -> Html Msg
viewBoard model =
    let
        custom =
            style
                [ ( "width", "810px" )
                , ( "height", "180px" )
                , ( "background-color", "gray" )
                ]
    in
        div [ custom ] (map translate model.board)


translate : Sect -> Html Msg
translate (Pos n col state container) =
    let
        color =
            case col of
                Red ->
                    "red"

                Black ->
                    "black"

                LBlue ->
                    "blue"
    in
        let
            custom =
                style
                    [ ( "height", "180px" )
                    , ( "width", "90px" )
                    , ( "float", "left" )
                    , ( "background-color", color )
                    ]
        in
            let
                click =
                    case col of
                        LBlue ->
                            TurnOff

                        _ ->
                            if state == Ready then
                                SendR n
                                -- will probably need a number
                            else
                                ChangeR n
            in
                div [ custom, onClick click ] []


sect : Color -> Int -> Html Msg
sect col n =
    div [ onClick (ChangeR n) ] []


sectReset : String -> Html Msg
sectReset str =
    div [ onClick (SendR 1) ] []



-- The gameboard needs to reflect the changes


transit : List Sect -> Action
transit xs =
    case xs of
        [] ->
            Turn

        x :: xs ->
            case x of
                Pos n LBlue Ready NoPiece ->
                    SendReset

                r ->
                    transit xs



--    case transit model.board of
--        Turn      -> div [] []
--        Change n  -> div [] []
--        SendReset -> div [] []
--        Other     -> div [] []
-- Update : the model is changed to reflect the change in state
-- You need to follow the model and view model
