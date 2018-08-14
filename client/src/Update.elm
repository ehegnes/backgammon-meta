module Update exposing (update)

import Types exposing (..)
import Model exposing (defBoard)


update : Msg -> Model -> Model
update msg model =
    case msg of
        TurnOff ->
            { model | board = defBoard }

        -- Update model for Turnoff
        ChangeR n ->
            prepare model n

        -- Update model for one key
        SendR n ->
            sendRequest model n



-- Update model back to turnoff and send http.request
--infixl 9 !!


(!!) : List a -> Int -> Maybe a
(!!) xs n =
    if n < 1 then
        Nothing
    else
        case ( xs, n ) of
            ( [], _ ) ->
                Nothing

            ( x :: xs, 1 ) ->
                Just x

            ( _ :: xs, n ) ->
                xs !! (n - 1)


compareSwap : Sect -> Sect -> Sect
compareSwap (Pos n col1 s1 cont1) (Pos m col2 s2 cont2) =
    if n == m then
        (Pos n LBlue Ready (Cont n col1))
    else
        (Pos n col1 Ready cont1)


recolor : Sect -> Sect -> Sect
recolor (Pos n col1 s1 (Cont n col22)) (Pos m col2 s2 cont) =
    if n == m then
        (Pos n col2 Not (Cont n col1))
        -- this needs to be adjusted
    else
        (Pos n col1 Not cont1)



-- this needs to be adjusted for LBlue


mapAlt : (Sect -> Sect -> Sect) -> Sect -> List Sect -> List Sect
mapAlt f n xs =
    case xs of
        [] ->
            []

        x :: xs ->
            (f x n) :: mapAlt f n xs


prepare : Model -> Int -> Model
prepare model n =
    let
        boardElt =
            case model.board !! n of
                Just m ->
                    m

                Nothing ->
                    Pos 0 Black Not NoPiece
    in
        let
            newBoard =
                mapAlt compareSwap boardElt model.board
        in
            { model | board = newBoard }


sendRequest : Model -> Int -> Model
sendRequest model n =
    let
        boardElt =
            case model.board !! n of
                Just m ->
                    m

                Nothing ->
                    Pos 0 Red Not NoPiece
    in
        let
            newBoard =
                mapAlt recolor boardElt model.board
        in
            { model | board = newBoard }
