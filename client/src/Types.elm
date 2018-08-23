module Types exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String

type Player
    = Black
    | White


decodePlayer : Decoder Player
decodePlayer =
    string
        |> andThen
            (\s ->
                case s of
                    "Black" ->
                        succeed Black

                    "White" ->
                        succeed White

                    e ->
                        fail <| "Unknown player: " ++ e
            )
