module Main exposing (..)

import Html exposing (beginnerProgram)
import Msgs exposing (Msg)
import Model exposing (Model, model)
import Update exposing (update)
import View exposing (view)



main : Program Never Model Msg
main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }
