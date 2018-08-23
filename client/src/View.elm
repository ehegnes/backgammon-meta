module View
    exposing
        ( view
        )

import Types exposing (..)
import Html exposing (Html, div, text)
import Msgs exposing (Msg)
import Model exposing (Model)


view : Model -> Html Msg
view model =
    div []
        [ text (toString model) ]
