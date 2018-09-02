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
   { board: (List (Maybe Point))
   , barBlack: Int
   , barWhite: Int
   }

jsonDecBoard : Json.Decode.Decoder ( Board )
jsonDecBoard =
   ("board" := Json.Decode.list (Json.Decode.maybe (jsonDecPoint))) >>= \pboard ->
   ("barBlack" := Json.Decode.int) >>= \pbarBlack ->
   ("barWhite" := Json.Decode.int) >>= \pbarWhite ->
   Json.Decode.succeed {board = pboard, barBlack = pbarBlack, barWhite = pbarWhite}

jsonEncBoard : Board -> Value
jsonEncBoard  val =
   Json.Encode.object
   [ ("board", (Json.Encode.list << List.map (maybeEncode (jsonEncPoint))) val.board)
   , ("barBlack", Json.Encode.int val.barBlack)
   , ("barWhite", Json.Encode.int val.barWhite)
   ]

