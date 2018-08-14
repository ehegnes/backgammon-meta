module Main exposing (..)

import Html exposing (beginnerProgram)

import Types exposing (..)
import Model exposing (defBoard, model)
import View  exposing (translate, view)
import Update exposing (update)

main : Program Never Model Msg
main =
    beginnerProgram
        { model = model
        , view  = view
        , update = update
        }


event : Model -> Action
event model = Turn
--      case (model.state, model.color) of
--          (Ready, LBlue) ->
--            TurnOff
--          (Not   , Red ) ->
--            Change
--	  (Ready , LBlue ) ->
--            SendReset
--	  (_ , _)     ->
--            Other

-- Construct HTML in pieces for later swapping

