module Types exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict
import Set


type Player  =
    Black 
    | White 

jsonDecPlayer : Json.Decode.Decoder ( Player )
jsonDecPlayer = 
    let jsonDecDictPlayer = Dict.fromList [("Black", Black), ("White", White)]
    in  decodeSumUnaries "Player" jsonDecDictPlayer

jsonEncPlayer : Player -> Value
jsonEncPlayer  val =
    case val of
        Black -> Json.Encode.string "Black"
        White -> Json.Encode.string "White"



type alias Point  =
   { owner: Player
   , count: Int
   }

jsonDecPoint : Json.Decode.Decoder ( Point )
jsonDecPoint =
   ("owner" := jsonDecPlayer) >>= \powner ->
   ("count" := Json.Decode.int) >>= \pcount ->
   Json.Decode.succeed {owner = powner, count = pcount}

jsonEncPoint : Point -> Value
jsonEncPoint  val =
   Json.Encode.object
   [ ("owner", jsonEncPlayer val.owner)
   , ("count", Json.Encode.int val.count)
   ]



type alias Board  =
   { board: InternalBoard
   , barBlack: Int
   , barWhite: Int
   }

jsonDecBoard : Json.Decode.Decoder ( Board )
jsonDecBoard =
   ("board" := jsonDecInternalBoard) >>= \pboard ->
   ("barBlack" := Json.Decode.int) >>= \pbarBlack ->
   ("barWhite" := Json.Decode.int) >>= \pbarWhite ->
   Json.Decode.succeed {board = pboard, barBlack = pbarBlack, barWhite = pbarWhite}

jsonEncBoard : Board -> Value
jsonEncBoard  val =
   Json.Encode.object
   [ ("board", jsonEncInternalBoard val.board)
   , ("barBlack", Json.Encode.int val.barBlack)
   , ("barWhite", Json.Encode.int val.barWhite)
   ]



type InternalBoard  =
    InternalBoard (List (Maybe Point))

jsonDecInternalBoard : Json.Decode.Decoder ( InternalBoard )
jsonDecInternalBoard =
    Json.Decode.lazy (\_ -> Json.Decode.map InternalBoard (Json.Decode.list (Json.Decode.maybe (jsonDecPoint))))


jsonEncInternalBoard : InternalBoard -> Value
jsonEncInternalBoard (InternalBoard v1) =
    (Json.Encode.list << List.map (maybeEncode (jsonEncPoint))) v1

