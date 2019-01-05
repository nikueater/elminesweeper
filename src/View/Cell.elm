module View.Cell exposing (view)

import Data.Color exposing (..)
import Data.Field exposing (..)
import Element exposing (..)
import Element.Background as Bg
import Element.Events as Events
import Element.Font as Font


view : (Id -> msg) -> Cell -> Element msg
view msg cell =
    case cell.state of
        Hidden ->
            cellHidden msg cell

        Opened ->
            cellOpened cell


type alias CellInfo =
    { color : Color
    , bgColor : Color
    , text : String
    }


cellHidden : (Id -> msg) -> Cell -> Element msg
cellHidden msg cell =
    cellElement
        [ Events.onClick (msg cell.id)
        ]
        (CellInfo white lightGray "")


cellOpened : Cell -> Element msg
cellOpened cell =
    let
        info =
            case cell.category of
                Plain 0 ->
                    CellInfo darkGray white ""

                Plain n ->
                    let
                        alpha =
                            0.1 * (toFloat n / 8.0)
                    in
                    CellInfo darkGray (withAlpha red alpha) (String.fromInt n)

                Mine ->
                    CellInfo red (withAlpha red 0.1) "X"
    in
    cellElement [] info


cellElement : List (Attribute msg) -> CellInfo -> Element msg
cellElement attr info =
    el
        [ width (px 48)
        , height (px 48)
        , padding 1
        ]
        (el
            ([ width fill
             , height fill
             , Font.color info.color
             , Bg.color info.bgColor
             ]
                ++ attr
            )
            (el [ centerY, centerX ] (text info.text))
        )
