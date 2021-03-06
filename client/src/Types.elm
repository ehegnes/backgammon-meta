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
   { _board: (List (Maybe Point))
   , barBlack: Int
   , barWhite: Int
   }

jsonDecBoard : Json.Decode.Decoder ( Board )
jsonDecBoard =
   ("_board" := Json.Decode.list (Json.Decode.maybe (jsonDecPoint))) >>= \p_board ->
   ("barBlack" := Json.Decode.int) >>= \pbarBlack ->
   ("barWhite" := Json.Decode.int) >>= \pbarWhite ->
   Json.Decode.succeed {_board = p_board, barBlack = pbarBlack, barWhite = pbarWhite}

jsonEncBoard : Board -> Value
jsonEncBoard  val =
   Json.Encode.object
   [ ("_board", (Json.Encode.list << List.map (maybeEncode (jsonEncPoint))) val._board)
   , ("barBlack", Json.Encode.int val.barBlack)
   , ("barWhite", Json.Encode.int val.barWhite)
   ]



type Submove  =
    SubmoveBearOff Int
    | SubmoveEnter Int
    | SubmoveMove Int Int

jsonDecSubmove : Json.Decode.Decoder ( Submove )
jsonDecSubmove =
    let jsonDecDictSubmove = Dict.fromList
            [ ("SubmoveBearOff", Json.Decode.lazy (\_ -> Json.Decode.map SubmoveBearOff (Json.Decode.int)))
            , ("SubmoveEnter", Json.Decode.lazy (\_ -> Json.Decode.map SubmoveEnter (Json.Decode.int)))
            , ("SubmoveMove", Json.Decode.lazy (\_ -> Json.Decode.map2 SubmoveMove (Json.Decode.index 0 (Json.Decode.int)) (Json.Decode.index 1 (Json.Decode.int))))
            ]
    in  decodeSumObjectWithSingleField  "Submove" jsonDecDictSubmove

jsonEncSubmove : Submove -> Value
jsonEncSubmove  val =
    let keyval v = case v of
                    SubmoveBearOff v1 -> ("SubmoveBearOff", encodeValue (Json.Encode.int v1))
                    SubmoveEnter v1 -> ("SubmoveEnter", encodeValue (Json.Encode.int v1))
                    SubmoveMove v1 v2 -> ("SubmoveMove", encodeValue (Json.Encode.list [Json.Encode.int v1, Json.Encode.int v2]))
    in encodeSumObjectWithSingleField keyval val



type Move  =
    Move (List Submove)

jsonDecMove : Json.Decode.Decoder ( Move )
jsonDecMove =
    Json.Decode.lazy (\_ -> Json.Decode.map Move (Json.Decode.list (jsonDecSubmove)))


jsonEncMove : Move -> Value
jsonEncMove (Move v1) =
    (Json.Encode.list << List.map jsonEncSubmove) v1



type Dice  =
    Dice (Int, Int)

jsonDecDice : Json.Decode.Decoder ( Dice )
jsonDecDice =
    Json.Decode.lazy (\_ -> Json.Decode.map Dice (Json.Decode.map2 (,) (Json.Decode.index 0 (Json.Decode.int)) (Json.Decode.index 1 (Json.Decode.int))))


jsonEncDice : Dice -> Value
jsonEncDice (Dice v1) =
    (\(v1,v2) -> Json.Encode.list [(Json.Encode.int) v1,(Json.Encode.int) v2]) v1



type alias Game  =
   { board: Board
   , dice: Dice
   , turn: Player
   }

jsonDecGame : Json.Decode.Decoder ( Game )
jsonDecGame =
   ("board" := jsonDecBoard) >>= \pboard ->
   ("dice" := jsonDecDice) >>= \pdice ->
   ("turn" := jsonDecPlayer) >>= \pturn ->
   Json.Decode.succeed {board = pboard, dice = pdice, turn = pturn}

jsonEncGame : Game -> Value
jsonEncGame  val =
   Json.Encode.object
   [ ("board", jsonEncBoard val.board)
   , ("dice", jsonEncDice val.dice)
   , ("turn", jsonEncPlayer val.turn)
   ]

