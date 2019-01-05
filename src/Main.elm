module Main exposing (Model, Msg(..), main, update, view)

import Browser exposing (Document, document)
import Data.Color exposing (..)
import Data.Field as Field exposing (..)
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import List.Extra as List
import Random
import Random.List exposing (shuffle)
import View.Cell as Cell


type alias Model =
    { title : String
    , field : Maybe Field
    , state : State
    }


type Msg
    = Initialize (List Int)
    | Clicked Id
    | Reset


type State
    = GameOver
    | Playing
    | Cleared


main : Program () Model Msg
main =
    document
        { init = \_ -> ( Model "elmine" Nothing GameOver, setMines Field.defaultSize )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( model, setMines Field.defaultSize )

        Initialize mines ->
            let
                field =
                    mines
                        |> List.take 5
                        |> Field.init Field.defaultSize
            in
            ( { model | field = Just field, state = Playing }, Cmd.none )

        Clicked id ->
            let
                field =
                    model.field
                        |> Maybe.map (Field.openAround id)

                state =
                    field
                        |> Maybe.map (checkState id)
                        |> Maybe.withDefault GameOver
            in
            ( { model | field = field, state = state }, Cmd.none )


checkState : Id -> Field -> State
checkState id field =
    if Field.isMine id field then
        GameOver

    else if Field.isCleared field then
        Cleared

    else
        Playing


setMines : Size -> Cmd Msg
setMines { w, h } =
    List.range 0 (w * h)
        |> shuffle
        |> Random.generate Initialize


view : Model -> Document Msg
view model =
    { title = model.title
    , body = List.singleton <| body model
    }


body : Model -> Html Msg
body model =
    row
        [ centerX
        , centerY
        ]
        [ viewStage model
        , viewForm model
        ]
        |> layout []


viewStage : Model -> Element Msg
viewStage model =
    let
        cover =
            case model.state of
                Playing ->
                    []

                s ->
                    [ inFront (viewCover s) ]
    in
    model.field
        |> Maybe.map viewField
        |> Maybe.withDefault (text "")
        |> el cover


viewField : Field -> Element Msg
viewField field =
    field.cells
        |> List.map (Cell.view Clicked)
        |> List.groupsOf field.size.w
        |> List.map (column [ pointer ])
        |> row []


viewForm : Model -> Element Msg
viewForm model =
    column
        [ alignTop
        , padding 1
        , width (px 96)
        ]
        [ Input.button
            [ width fill
            , height (px 48)
            , Border.color lightGray
            , Border.width 1
            , Font.center
            ]
            { onPress = Just Reset, label = text "reset" }
        ]


viewCover : State -> Element Msg
viewCover state =
    let
        coverText =
            case state of
                GameOver ->
                    "GAME OVER"

                Playing ->
                    ""

                Cleared ->
                    "CLEARED"
    in
    el
        [ width fill
        , height fill
        , Bg.color (withAlpha darkGray 0.5)
        ]
        (el
            [ centerX
            , centerY
            , Font.size 32
            ]
            (text coverText)
        )
