module Main exposing (main)

import Array exposing (fromList)
import Browser
import Game exposing (GameState, InGameMsg(..), startGame, updateGame, viewGame)
import Html exposing (..)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)
import List exposing (map)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = sub }



-- MODEL


type alias Model =
    { width : Int
    , height : Int
    , numMines : Int
    , message : Maybe ( InputField, String )
    , gameState : Maybe GameState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { width = 9
      , height = 9
      , numMines = 13
      , message = Nothing
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
    , minNumMines = \width height -> ceiling <| toFloat (width * height // 8)
    , standardNumMines = \width height -> width * height // 6
    , maxNumMines = \width height -> width * height // 4
    }


updateInput : InputMsg -> Model -> ( Model, Cmd Msg )
updateInput msg model =
    case msg of
        Increment field ->
            ( case field of
                Width ->
                    if model.width + 1 > defaultValidation.maxWidth then
                        { model
                            | width = defaultValidation.maxWidth
                            , numMines =
                                defaultValidation.standardNumMines defaultValidation.maxWidth model.height
                            , message =
                                Just
                                    ( Width
                                    , "The maximum width allowed is " ++ String.fromInt defaultValidation.maxWidth
                                    )
                        }

                    else
                        { model
                            | width = model.width + 1
                            , message = Nothing
                        }

                Height ->
                    if model.height + 1 > defaultValidation.maxHeight then
                        { model
                            | height = defaultValidation.maxHeight
                            , numMines =
                                defaultValidation.standardNumMines model.width defaultValidation.maxHeight
                            , message =
                                Just
                                    ( Height
                                    , "The maximum height allowed is " ++ String.fromInt defaultValidation.maxHeight
                                    )
                        }

                    else
                        { model
                            | height = model.height + 1
                            , message = Nothing
                        }

                NumMines ->
                    if (model.numMines + 1) > defaultValidation.maxNumMines model.width model.height then
                        { model
                            | numMines = defaultValidation.maxNumMines model.width model.height
                            , message =
                                Just
                                    ( NumMines
                                    , "You can't have more than "
                                        ++ String.fromInt (defaultValidation.maxNumMines model.width model.height)
                                        ++ " mines for this grid size - it would be too hard!"
                                    )
                        }

                    else
                        { model | numMines = model.numMines + 1, message = Nothing }
            , Cmd.none
            )

        Decrement field ->
            ( case field of
                Width ->
                    if model.width - 1 < defaultValidation.minWidth then
                        { model
                            | width = defaultValidation.minWidth
                            , numMines =
                                defaultValidation.standardNumMines defaultValidation.minWidth model.height
                            , message =
                                Just
                                    ( Width
                                    , "The minimum width allowed is " ++ String.fromInt defaultValidation.minWidth
                                    )
                        }

                    else
                        { model
                            | width = model.width - 1
                            , message = Nothing
                        }

                Height ->
                    if model.height - 1 < defaultValidation.minHeight then
                        { model
                            | height = defaultValidation.minHeight
                            , numMines =
                                defaultValidation.standardNumMines model.width defaultValidation.minHeight
                            , message =
                                Just
                                    ( Height
                                    , "The minimum height allowed is " ++ String.fromInt defaultValidation.minHeight
                                    )
                        }

                    else
                        { model
                            | height = model.height - 1
                            , message = Nothing
                        }

                NumMines ->
                    if (model.numMines - 1) > defaultValidation.minNumMines model.width model.height then
                        { model
                            | numMines = defaultValidation.minNumMines model.width model.height
                            , message =
                                Just
                                    ( NumMines
                                    , "You can't have fewer than "
                                        ++ String.fromInt (defaultValidation.minNumMines model.width model.height)
                                        ++ " mines for this grid size - it would be too easy!"
                                    )
                        }

                    else
                        { model | numMines = model.numMines - 1, message = Nothing }
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

                maxVal =
                    case field of
                        Width ->
                            defaultValidation.maxWidth

                        Height ->
                            defaultValidation.maxHeight

                        NumMines ->
                            defaultValidation.maxNumMines model.width model.height

                minVal =
                    case field of
                        Width ->
                            defaultValidation.minWidth

                        Height ->
                            defaultValidation.minHeight

                        NumMines ->
                            defaultValidation.minNumMines model.width model.height

                maxMessage =
                    case field of
                        Width ->
                            "The maximum width allowed is " ++ String.fromInt maxVal

                        Height ->
                            "The maximum height allowed is " ++ String.fromInt maxVal

                        NumMines ->
                            "You can't have more than "
                                ++ String.fromInt maxVal
                                ++ " mines for this grid size - it would be too hard!"

                minMessage =
                    case field of
                        Width ->
                            "The minimum width allowed is " ++ String.fromInt minVal

                        Height ->
                            "The minimum height allowed is " ++ String.fromInt minVal

                        NumMines ->
                            "You can't have fewer than "
                                ++ String.fromInt minVal
                                ++ " mines for this grid size - it would be too easy!"
            in
            ( case field of
                Width ->
                    if val > maxVal then
                        { model | width = maxVal, message = Just ( Width, maxMessage ) }

                    else if val < minVal then
                        { model | width = minVal, message = Just ( Width, minMessage ) }

                    else
                        { model | width = val, message = Nothing }

                Height ->
                    if val > maxVal then
                        { model | height = maxVal, message = Just ( Height, maxMessage ) }

                    else if val < minVal then
                        { model | height = minVal, message = Just ( Height, minMessage ) }

                    else
                        { model | height = val, message = Nothing }

                NumMines ->
                    if val > maxVal then
                        { model | numMines = maxVal, message = Just ( NumMines, maxMessage ) }

                    else if val < minVal then
                        { model | numMines = minVal, message = Just ( NumMines, minMessage ) }

                    else
                        { model | numMines = val, message = Nothing }
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


viewField : InputField -> Int -> Maybe ( InputField, String ) -> Html InputMsg
viewField field val maybeErr =
    let
        fieldText =
            case field of
                Width ->
                    "Grid Width"

                Height ->
                    "Grid Height"

                NumMines ->
                    "Number of Mines"

        errElements =
            case maybeErr of
                Nothing ->
                    []

                Just ( errField, errMsg ) ->
                    if errField == field then
                        [ div [ class "error" ] [ text errMsg ] ]

                    else
                        []
    in
    div [ class "single-option" ] <|
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
            ++ errElements


viewInput : Int -> Int -> Int -> Maybe ( InputField, String ) -> Html InputMsg
viewInput width height numMines maybeErr =
    div [ class "game-options" ] <|
        (++)
            (h3 [ class "form-title" ] [ text "Game Options" ]
                :: map (\( field, val ) -> viewField field val maybeErr)
                    [ ( Width, width ), ( Height, height ), ( NumMines, numMines ) ]
            )
        <|
            [ button [ class "main-button", onClick StartGame ] [ text "Start Game" ] ]


view : Model -> Html Msg
view model =
    div [] <|
        (case model.gameState of
            Nothing ->
                Html.map Input <| viewInput model.width model.height model.numMines model.message

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
