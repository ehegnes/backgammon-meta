module Update
    exposing
        ( update
        )

import GeneratedTypes exposing (..)
import Types exposing (..)
import Model exposing (..)
import Msgs exposing (..)


update : Msg -> Model -> Model
update msg model =
    case msg of
        Default -> model
