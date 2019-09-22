module Main exposing (Model, Msg(..), init, main, realGrid, startGrid, update, view, viewCell, viewRow)

import Array exposing (Array, fromList, repeat, toList)
import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List exposing (map)
import Logic exposing (uncover)
import Types exposing (Grid, RealGrid, RealStatus(..), Status(..))


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { width : Int, height : Int, gridState : Grid }



-- later will randomly generate grid, but lets start with a fixed one for now


realGrid : RealGrid
realGrid =
    fromList
        (map fromList
            [ [ Safe, Mine, Safe, Safe, Safe, Safe, Mine, Safe, Safe ]
            , [ Safe, Safe, Safe, Safe, Safe, Safe, Mine, Safe, Safe ]
            , [ Mine, Mine, Safe, Safe, Safe, Mine, Safe, Safe, Safe ]
            , [ Safe, Safe, Safe, Safe, Mine, Safe, Mine, Mine, Mine ]
            , [ Safe, Safe, Safe, Mine, Safe, Safe, Safe, Safe, Safe ]
            , [ Mine, Mine, Safe, Safe, Mine, Safe, Safe, Safe, Mine ]
            , [ Safe, Mine, Safe, Safe, Safe, Safe, Safe, Mine, Mine ]
            , [ Safe, Safe, Mine, Safe, Safe, Safe, Safe, Safe, Safe ]
            , [ Safe, Mine, Mine, Safe, Safe, Safe, Mine, Safe, Safe ]
            ]
        )


startGrid : Int -> Int -> Grid
startGrid width height =
    repeat height (repeat width Unknown)


init : Model
init =
    { width = 9, height = 9, gridState = startGrid 9 9 }



-- UPDATE


type Msg
    = Click Int Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Click x y ->
            { model | gridState = uncover realGrid model.gridState ( x, y ) }



-- VIEW


viewCell : ( Int, Int ) -> Status -> Html Msg
viewCell ( x, y ) stat =
    let
        content status =
            case status of
                Unknown ->
                    []

                KnownMine ->
                    [ text "X" ]

                Neighbours n ->
                    [ text (String.fromInt n) ]
    in
    div
        [ style "display" "inline-block"
        , style "width" "20px"
        , style "height" "20px"
        , style "background-color" "#aaa"
        , style "color" "black"
        , style "margin-right" "2px"
        , style "margin-bottom" "2px"
        , style "overflow" "hidden"
        , style "line-height" "1.3"
        , style "text-align" "center"
        , onClick (Click x y)
        ]
        (content stat)


viewRow : Int -> Int -> Array Status -> Html Msg
viewRow width y cells =
    cells
        |> Array.indexedMap (\x status -> viewCell ( x, y ) status)
        |> toList
        |> div []


view : Model -> Html Msg
view model =
    model.gridState |> Array.indexedMap (\y status -> viewRow model.width y status) |> toList |> div []
