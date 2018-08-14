module Types exposing (..)


type Result
    = Win
    | Loss
    | Nei


type Turn
    = P1
    | P2
    | P0


type State
    = Ready
    | Not


type Color
    = Red
    | Black
    | LBlue


type Action
    = Turn
    | Change Int
    | SendReset
    | Other


type Msg
    = TurnOff
    | ChangeR Int
    | SendR Int


type alias Model =
    { board : List Sect
    , result : Result
    , turn : Turn
    , valid : List ( Int, Int )
    }


type Container
    = Cont Int Color
    | NoPiece


type Sect
    = Pos Int Color State Container


type BGSize
    = B18
