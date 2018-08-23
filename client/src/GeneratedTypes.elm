module GeneratedTypes exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


import Types exposing (..)

type alias Board =
    { board : InternalBoard
    , barBlack : Int
    , barWhite : Int
    }

decodeBoard : Decoder Board
decodeBoard =
    decode Board
        |> required "board" decodeInternalBoard
        |> required "barBlack" int
        |> required "barWhite" int

type alias InternalBoard =
    { internalBoard : List (Maybe (Point))
    }

decodeInternalBoard : Decoder InternalBoard
decodeInternalBoard =
    decode InternalBoard
        |> required "internalBoard" (list (maybe decodePoint))

type alias Point =
    { owner : Player
    , count : Int
    }

decodePoint : Decoder Point
decodePoint =
    decode Point
        |> required "owner" decodePlayer
        |> required "count" int

getBoard : Http.Request (Board)
getBoard =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "board"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeBoard
        , timeout =
            Nothing
        , withCredentials =
            False
        }