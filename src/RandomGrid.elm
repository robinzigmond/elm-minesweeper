module RandomGrid exposing (randomGrid, repeatUntilSafe)

import Array
import Logic exposing (reveal)
import Random exposing (Generator)
import Random.List exposing (shuffle)
import Types exposing (RealGrid, RealStatus(..))


randomGrid : Int -> Int -> Int -> Generator RealGrid
randomGrid width height numMines =
    let
        oneDimensional =
            shuffle <|
                List.repeat numMines Mine
                    ++ List.repeat (width * height - numMines) Safe

        split status arrs =
            case Array.length arrs of
                0 ->
                    Array.fromList [ Array.fromList [ status ] ]

                len ->
                    case Array.get (len - 1) arrs of
                        Nothing ->
                            arrs

                        Just lastArr ->
                            case Array.length lastArr of
                                w ->
                                    if w == width then
                                        Array.push (Array.fromList [ status ]) arrs

                                    else
                                        let
                                            update i arr =
                                                if i == (len - 1) then
                                                    Array.push status arr

                                                else
                                                    arr
                                        in
                                        Array.indexedMap update arrs
    in
    Random.map (List.foldl split Array.empty) oneDimensional


repeatUntilSafe : ( Int, Int ) -> Int -> Int -> Int -> Generator RealGrid
repeatUntilSafe clicked width height numMines =
    let
        repeat g =
            if reveal g clicked == Just Mine then
                repeat g

            else
                Random.constant g
    in
    Random.andThen repeat (randomGrid width height numMines)
