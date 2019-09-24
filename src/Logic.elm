module Logic exposing (countPossibleMines, reveal, uncover, insert, lookup)

import Array exposing (Array, get, set)
import List exposing (filter, length, map)
import Set exposing (fromList, toList)
import Types exposing (Grid, RealGrid, RealStatus(..), Status(..))


countPossibleMines : Grid -> Int
countPossibleMines g =
    g
        |> Array.map (Array.filter (\a -> a == Unknown || a == KnownMine) >> Array.length)
        |> Array.foldl (+) 0


reveal : RealGrid -> ( Int, Int ) -> Maybe RealStatus
reveal grid ( x, y ) =
    case get y grid of
        Nothing ->
            Nothing

        Just row ->
            get x row


lookup : Grid -> ( Int, Int ) -> Maybe Status
lookup grid ( x, y ) =
    case get y grid of
        Nothing ->
            Nothing

        Just row ->
            get x row


insert : ( Int, Int ) -> a -> Array (Array a) -> Array (Array a)
insert ( x, y ) a grid =
    case get y grid of
        Nothing ->
            grid

        Just row ->
            case get x row of
                Nothing ->
                    grid

                Just _ ->
                    set y (set x a row) grid


neighbours : ( Int, Int ) -> List ( Int, Int )
neighbours ( x, y ) =
    [ ( x - 1, y - 1 )
    , ( x - 1, y )
    , ( x - 1, y + 1 )
    , ( x, y - 1 )
    , ( x, y )
    , ( x, y + 1 )
    , ( x + 1, y - 1 )
    , ( x + 1, y )
    , ( x + 1, y + 1 )
    ]


countNeighbours : RealGrid -> ( Int, Int ) -> Int
countNeighbours grid ( x, y ) =
    neighbours ( x, y ) |> map (reveal grid) |> filter (\res -> res == Just Mine) |> length


uncoverMany : RealGrid -> Grid -> ( Int, Int ) -> List ( Int, Int )
uncoverMany grid found ( x, y ) =
    if lookup found ( x, y ) == Just Unknown then
        case reveal grid ( x, y ) of
            Nothing ->
                []

            Just Mine ->
                []

            Just Safe ->
                let
                    go next cum =
                        if List.isEmpty next then
                            cum

                        else
                            let
                                newNeighbours =
                                    filter
                                        (\pos ->
                                            reveal grid pos
                                                == Just Safe
                                                && not (List.member pos cum)
                                        )
                                        (List.concatMap neighbours
                                            (filter (\pos -> countNeighbours grid pos == 0) next)
                                        )
                            in
                            go newNeighbours ((next ++ cum) |> fromList |> toList)
                in
                go (filter (\pos -> reveal grid pos /= Nothing) (neighbours ( x, y ))) [ ( x, y ) ]

    else
        []


uncover : RealGrid -> Grid -> ( Int, Int ) -> Grid
uncover grid found ( x, y ) =
    case reveal grid ( x, y ) of
        Nothing ->
            found

        Just Mine ->
            case get y found of
                Nothing ->
                    found

                Just row ->
                    insert ( x, y ) KnownMine found

        Just Safe ->
            case countNeighbours grid ( x, y ) of
                0 ->
                    List.foldl
                        (\( x0, y0 ) gr ->
                            insert ( x0, y0 ) (Neighbours (countNeighbours grid ( x0, y0 ))) gr
                        )
                        found
                        (uncoverMany grid found ( x, y ))

                n ->
                    insert ( x, y ) (Neighbours n) found
