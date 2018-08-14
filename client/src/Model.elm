module Model exposing (defBoard, model)

import Types exposing (..)

genBoard : BGSize -> List Sect
genBoard B18 = buildBoard 18 18

buildBoard : Int -> Int -> List Sect
buildBoard n maxr =
    case n of
        0 -> []
        n -> let col = case (n % 2) of
                          0 -> Red
                          _ -> Black in
             let elt = Pos (maxr - (n - 1)) col Not NoPiece in
             let pos = elt in
                 pos :: buildBoard (n - 1) maxr

defBoard : List Sect
defBoard = genBoard B18
                 
model : Model
model =
    { board  = defBoard
    , result = Nei
    , turn   = P0
    , valid  = [(5, 6)]
    }
