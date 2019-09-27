module Main exposing (main)

import Array exposing (fromList)
import Browser
import Game exposing (GameState, InGameMsg(..), startGame, updateGame, viewGame)
import Html exposing (..)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput)
import List exposing (map)
import Types exposing (Grid, RealGrid, RealStatus(..), Status(..))


main =
    Browser.element { init = init, update = update, view = view, subscriptions = sub }



-- MODEL


type alias Model =
    { width : Int
    , height : Int
    , numMines : Int
    , error : Maybe String
    , gameState : Maybe GameState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { width = 9
      , height = 9
      , numMines = 13
      , gameState = Nothing
      , error = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type InputField
    = Width
    | Height
    | NumMines


type InputMsg
    = Increment InputField
    | Decrement InputField
    | New InputField Int
    | StartGame
    | Menu
    | Error String


type Msg
    = InGame InGameMsg
    | Input InputMsg


updateInput : InputMsg -> Model -> ( Model, Cmd Msg )
updateInput msg model =
    case msg of
        Increment field ->
            ( case field of
                Width ->
                    { model | width = model.width + 1 }

                Height ->
                    { model | height = model.height + 1 }

                NumMines ->
                    { model | numMines = model.numMines + 1 }
            , Cmd.none
            )

        Decrement field ->
            ( case field of
                Width ->
                    { model | width = model.width - 1 }

                Height ->
                    { model | height = model.height - 1 }

                NumMines ->
                    { model | numMines = model.numMines - 1 }
            , Cmd.none
            )

        New field val ->
            ( case field of
                Width ->
                    { model | width = val }

                Height ->
                    { model | height = val }

                NumMines ->
                    { model | numMines = val }
            , Cmd.none
            )

        Error errMsg ->
            ( { model | error = Just errMsg }, Cmd.none )

        StartGame ->
            let
                ( startState, cmd ) =
                    updateGame model.width model.height model.numMines NewGame <|
                        startGame model.width model.height model.numMines
            in
            ( { model | gameState = Just startState }, Cmd.map InGame cmd )

        Menu ->
            ( { model | gameState = Nothing }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InGame gameMsg ->
            case model.gameState of
                Just state ->
                    let
                        ( newModel, cmd ) =
                            updateGame model.width model.height model.numMines gameMsg state
                    in
                    ( { model | gameState = Just newModel }
                    , Cmd.map InGame cmd
                    )

                Nothing ->
                    ( model, Cmd.none )

        Input inputMsg ->
            updateInput inputMsg model



-- VIEW


viewField : InputField -> Int -> Html InputMsg
viewField field val =
    let
        fieldText =
            case field of
                Width ->
                    "Grid Width"

                Height ->
                    "Grid Height"

                NumMines ->
                    "Number of Mines"
    in
    div []
        [ text fieldText
        , button [ onClick <| Decrement field ] [ text "-" ]
        , input
            [ type_ "number"
            , value <| String.fromInt val
            , onInput <| New field << Maybe.withDefault 0 << String.toInt
            ]
            []
        , button [ onClick <| Increment field ] [ text "+" ]
        ]


viewInput : Int -> Int -> Int -> Maybe String -> Html InputMsg
viewInput width height numMines maybeErr =
    div [] <|
        (++)
            (map (\( field, val ) -> viewField field val)
                [ ( Width, width ), ( Height, height ), ( NumMines, numMines ) ]
            )
        <|
            [ button [ onClick StartGame ] [ text "Start Game" ] ]


view : Model -> Html Msg
view model =
    div [] <|
        (case model.gameState of
            Nothing ->
                Html.map Input <| viewInput model.width model.height model.numMines model.error

            Just game ->
                Html.map InGame <| viewGame game
        )
            :: (if model.gameState == Nothing then
                    []

                else
                    [ button [ onClick (Input Menu), class "new-game" ] [ text "Change game options" ] ]
               )



-- SUBSCRIPTIONS


sub : model -> Sub msg
sub _ =
    Sub.none
