module Data.Field exposing
    ( Cell
    , CellCategory(..)
    , CellState(..)
    , Field
    , Id
    , Size
    , defaultSize
    , init
    , isCleared
    , isMine
    , openAround
    )

import List.Extra exposing (count, groupsOf, updateIf)


type alias Risk =
    Int


type alias Id =
    Int


type alias Field =
    { size : Size
    , cells : List Cell
    }


type alias Size =
    { w : Int
    , h : Int
    }


type alias Pos =
    { x : Int
    , y : Int
    }


type alias Cell =
    { id : Int
    , category : CellCategory
    , state : CellState
    }


type CellState
    = Hidden
    | Opened


type CellCategory
    = Plain Risk
    | Mine


defaultSize : Size
defaultSize =
    Size 10 10


init : Size -> List Int -> Field
init size mines =
    { size = size
    , cells =
        List.range 0 (size.w * size.h - 1)
            |> List.map (generateCell mines size)
    }


surroundings : Size -> Id -> List Id
surroundings size index =
    let
        pos =
            Pos (remainderBy size.w index) (index // size.w)
    in
    [ Pos (pos.x - 1) (pos.y - 1)
    , Pos pos.x (pos.y - 1)
    , Pos (pos.x + 1) (pos.y - 1)
    , Pos (pos.x - 1) pos.y
    , Pos (pos.x + 1) pos.y
    , Pos (pos.x - 1) (pos.y + 1)
    , Pos pos.x (pos.y + 1)
    , Pos (pos.x + 1) (pos.y + 1)
    ]
        |> List.filter (\{ x } -> 0 <= x && x < size.w)
        |> List.filter (\{ y } -> 0 <= y && y < size.h)
        |> List.map (\{ x, y } -> y * size.w + x)


generateCell : List Int -> Size -> Int -> Cell
generateCell mines size index =
    case List.member index mines of
        True ->
            Cell index Mine Hidden

        False ->
            let
                risk =
                    surroundings size index
                        |> count (\x -> List.member x mines)
            in
            Cell index (Plain risk) Hidden


open : Id -> Field -> Field
open index field =
    let
        cells =
            field.cells
                |> updateIf (.id >> (==) index) (\cell -> { cell | state = Opened })
    in
    { field | cells = cells }


isMine : Id -> Field -> Bool
isMine index field =
    field.cells
        |> List.filter (.id >> (==) index)
        |> List.head
        |> Maybe.map (.category >> (==) Mine)
        |> Maybe.withDefault False


isCleared : Field -> Bool
isCleared =
    .cells
        >> List.filter (.state >> (==) Hidden)
        >> List.all (.category >> (==) Mine)


openAround : Id -> Field -> Field
openAround index field =
    let
        target =
            field.cells
                |> List.filter (.id >> (==) index)
                |> List.head

        check cell =
            case ( cell.category, cell.state ) of
                ( Plain 0, Hidden ) ->
                    surroundings field.size index
                        |> List.foldl (\id fd -> openAround id fd) (open index field)

                _ ->
                    open index field
    in
    target
        |> Maybe.map check
        |> Maybe.withDefault field
