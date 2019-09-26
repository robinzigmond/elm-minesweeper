module Main exposing (main)

import Array exposing (Array, fromList, repeat, toList)
import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (custom, onClick)
import Json.Decode as Decode
import List exposing (map)
import Logic exposing (countKnownMines, countPossibleMines, insert, lookup, reveal, uncover)
import Random exposing (generate)
import RandomGrid exposing (randomGrid, repeatUntilSafe)
import Types exposing (Grid, RealGrid, RealStatus(..), Status(..))


main =
    Browser.element { init = init, update = update, view = view, subscriptions = sub }



-- MODEL


type alias Model =
    { playing : Bool
    , won : Bool
    , width : Int
    , height : Int
    , numMines : Int
    , firstTurn : Bool
    , gridState : Grid
    , realGrid : RealGrid
    }


startGrid : Int -> Int -> Grid
startGrid width height =
    repeat height (repeat width Unknown)


startRealGrid : Int -> Int -> RealGrid
startRealGrid width height =
    repeat height (repeat width Safe)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { playing = True
      , won = False
      , width = 9
      , height = 9
      , numMines = 13
      , firstTurn = True
      , gridState = startGrid 9 9
      , realGrid = startRealGrid 9 9
      }
    , Random.generate NewGrid (randomGrid 9 9 13)
    )



-- UPDATE


type Msg
    = Click Int Int
    | NewGrid RealGrid
    | Substitute Int Int RealGrid
    | NewGame
    | ToggleMine Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click x y ->
            let
                result =
                    uncover model.realGrid model.gridState ( x, y )

                hasWon =
                    countPossibleMines result == model.numMines

                isMine =
                    reveal model.realGrid ( x, y ) == Just Mine

                redo =
                    model.firstTurn && isMine
            in
            if redo then
                ( { model | gridState = result, firstTurn = False }
                , Random.generate (Substitute x y) (repeatUntilSafe ( x, y ) 9 9 13)
                )

            else if isMine then
                if lookup model.gridState ( x, y ) == Just KnownMine then
                    ( model, Cmd.none )

                else
                    ( { model | gridState = result, firstTurn = False, playing = False }
                    , Random.generate NewGrid (randomGrid 9 9 13)
                    )

            else
                ( { model | gridState = result, firstTurn = False, won = hasWon }, Cmd.none )

        ToggleMine x y ->
            let
                current =
                    lookup model.gridState ( x, y )

                toggleResult =
                    case current of
                        Just Unknown ->
                            KnownMine

                        Just KnownMine ->
                            Unknown

                        Just state ->
                            state

                        Nothing ->
                            KnownMine
            in
            ( { model | gridState = insert ( x, y ) toggleResult model.gridState }, Cmd.none )

        NewGrid newGrid ->
            ( { model | realGrid = newGrid, firstTurn = True }, Cmd.none )

        Substitute x y newGrid ->
            ( { model | realGrid = newGrid, gridState = uncover newGrid (startGrid 9 9) ( x, y ) }
            , Cmd.none
            )

        NewGame ->
            ( { model | gridState = startGrid 9 9, firstTurn = True, playing = True, won = False }
            , Random.generate NewGrid (randomGrid 9 9 13)
            )



-- VIEW


onRightClick : msg -> Attribute msg
onRightClick message =
    custom "contextmenu" (Decode.succeed { message = message, preventDefault = True, stopPropagation = False })


viewCell : Bool -> ( Int, Int ) -> Status -> Html Msg
viewCell playing ( x, y ) stat =
    let
        content status =
            case status of
                Unknown ->
                    []

                KnownMine ->
                    [ text "X" ]

                Neighbours n ->
                    [ text (String.fromInt n) ]

        actions =
            if playing then
                [ onClick (Click x y), onRightClick (ToggleMine x y) ]

            else
                []

        otherClass =
            case stat of
                Unknown ->
                    " empty"

                KnownMine ->
                    " mine"

                _ ->
                    ""
    in
    div
        (class
            ("cell" ++ otherClass)
            :: actions
        )
        (content stat)


viewRow : Bool -> Int -> Int -> Array Status -> Html Msg
viewRow playing width y cells =
    cells
        |> Array.indexedMap (\x status -> viewCell playing ( x, y ) status)
        |> toList
        |> div []


view : Model -> Html Msg
view model =
    let
        mineCounter =
            p [ class "mines-remaining" ]
                [ text "Mines Remaining "
                , span [ class "mine-count" ] <|
                    map (\c -> span [ class "digit" ] [ text (String.fromChar c) ]) <|
                        String.toList
                            (String.padLeft 3 '0' <|
                                String.fromInt (model.numMines - countKnownMines model.gridState)
                            )
                ]

        grid =
            model.gridState
                |> Array.indexedMap (\y status -> viewRow model.playing model.width y status)
                |> toList
                |> (\l -> mineCounter :: l)
                |> div [ class "game" ]

        newGameButton =
            button [ onClick NewGame, class "new-game" ] [ text "New Game" ]

        failureMessage =
            div [ class "notice failure" ] [ text "Sorry you lost the game!" ]

        winMessage =
            div [ class "notice win" ] [ text "Well done, you uncovered everything but the mines!" ]

        elements =
            if model.playing then
                if model.won then
                    [ grid, winMessage, newGameButton ]

                else
                    [ grid, newGameButton ]

            else
                [ grid, failureMessage, newGameButton ]
    in
    div [ class "container" ] elements



-- SUBSCRIPTIONS


sub : model -> Sub msg
sub _ =
    Sub.none
