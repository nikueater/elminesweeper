module Data.Color exposing (darkGray, lightGray, red, white, withAlpha)

import Element exposing (Color, rgb, rgba, toRgb)


white : Color
white =
    rgb 1 1 1


lightGray : Color
lightGray =
    rgb 0.9 0.9 0.9


darkGray : Color
darkGray =
    rgb 0.4 0.4 0.5


red : Color
red =
    rgb 1 0 0


withAlpha : Color -> Float -> Color
withAlpha color alpha =
    color
        |> toRgb
        |> (\c -> rgba c.red c.green c.blue alpha)
