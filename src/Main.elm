module Main exposing (main)

import Array exposing (fromList)
import Browser
import Game exposing (GameState, InGameMsg(..), startGame, updateGame, viewGame)
import Html exposing (..)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)
import List exposing (map)
import Types exposing (Grid, RealGrid, RealStatus(..), Status(..))


main =
    Browser.element { init = init, update = update, view = view, subscriptions = sub }



-- MODEL


type alias Model =
    { width : Int
    , height : Int
    , numMines : Int
    , gameState : Maybe GameState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { width = 9
      , height = 9
      , numMines = 13
      , gameState = Nothing
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
    | CheckMaxMin InputField
    | StartGame
    | Menu


type Msg
    = InGame InGameMsg
    | Input InputMsg


type alias ValidationData =
    { minWidth : Int
    , maxWidth : Int
    , minHeight : Int
    , maxHeight : Int
    , minNumMines : Int -> Int -> Int
    , maxNumMines : Int -> Int -> Int
    , standardNumMines : Int -> Int -> Int
    }


defaultValidation : ValidationData
defaultValidation =
    { minWidth = 5
    , maxWidth = 25
    , minHeight = 5
    , maxHeight = 25
    , minNumMines = \width height -> (width * height // 8) + 1
    , standardNumMines = \width height -> width * height // 6
    , maxNumMines = \width height -> width * height // 4
    }


updateInput : InputMsg -> Model -> ( Model, Cmd Msg )
updateInput msg model =
    case msg of
        Increment field ->
            ( case field of
                Width ->
                    let
                        newVal =
                            min defaultValidation.maxWidth <| model.width + 1
                    in
                    { model
                        | width = newVal
                        , numMines = defaultValidation.standardNumMines newVal model.height
                    }

                Height ->
                    let
                        newVal =
                            min defaultValidation.maxHeight <| model.height + 1
                    in
                    { model
                        | height = newVal
                        , numMines = defaultValidation.standardNumMines model.width newVal
                    }

                NumMines ->
                    { model
                        | numMines =
                            min (defaultValidation.maxNumMines model.width model.height) <|
                                model.numMines
                                    + 1
                    }
            , Cmd.none
            )

        Decrement field ->
            ( case field of
                Width ->
                    let
                        newVal =
                            max defaultValidation.minWidth <| model.width - 1
                    in
                    { model
                        | width = newVal
                        , numMines = defaultValidation.standardNumMines newVal model.height
                    }

                Height ->
                    let
                        newVal =
                            max defaultValidation.minHeight <| model.height - 1
                    in
                    { model
                        | height = newVal
                        , numMines = defaultValidation.standardNumMines model.width newVal
                    }

                NumMines ->
                    { model
                        | numMines =
                            max (defaultValidation.minNumMines model.width model.height) <|
                                model.numMines
                                    - 1
                    }
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

        CheckMaxMin field ->
            let
                val =
                    case field of
                        Width ->
                            model.width

                        Height ->
                            model.height

                        NumMines ->
                            model.numMines
            in
            ( case field of
                Width ->
                    let
                        newVal =
                            max defaultValidation.minWidth <| min defaultValidation.maxWidth val
                    in
                    { model
                        | width = newVal
                        , numMines = defaultValidation.standardNumMines newVal model.height
                    }

                Height ->
                    let
                        newVal =
                            max defaultValidation.minHeight <| min defaultValidation.maxHeight val
                    in
                    { model
                        | height = newVal
                        , numMines = defaultValidation.standardNumMines model.width newVal
                    }

                NumMines ->
                    { model
                        | numMines =
                            max (defaultValidation.minNumMines model.width model.height) <|
                                min (defaultValidation.maxNumMines model.width model.height) val
                    }
            , Cmd.none
            )

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
    div [ class "single-option" ]
        [ p [ class "field-label" ] [ text fieldText ]
        , div [ class "input-container" ]
            [ button [ class "input-btn", onClick <| Decrement field ] [ text "-" ]
            , input
                [ type_ "number"
                , value <| String.fromInt val
                , onBlur (CheckMaxMin field)
                , onInput <| New field << Maybe.withDefault 0 << String.toInt
                ]
                []
            , button [ class "input-btn", onClick <| Increment field ] [ text "+" ]
            ]
        ]


viewInput : Int -> Int -> Int -> Html InputMsg
viewInput width height numMines =
    div [ class "game-options" ] <|
        (++)
            (h3 [ class "form-title" ] [ text "Game Options" ]
                :: map (\( field, val ) -> viewField field val)
                    [ ( Width, width ), ( Height, height ), ( NumMines, numMines ) ]
            )
        <|
            [ button [ class "main-button", onClick StartGame ] [ text "Start Game" ] ]


view : Model -> Html Msg
view model =
    div [] <|
        (case model.gameState of
            Nothing ->
                Html.map Input <| viewInput model.width model.height model.numMines

            Just game ->
                Html.map InGame <| viewGame game
        )
            :: (if model.gameState == Nothing then
                    []

                else
                    [ button [ onClick (Input Menu), class "main-button" ] [ text "Change game options" ] ]
               )



-- SUBSCRIPTIONS


sub : model -> Sub msg
sub _ =
    Sub.none
