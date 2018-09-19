module Petals exposing (main)

{-| Based on the arrangement of petals in a sunflower. Demonstrates the initial layout of Force.entity.
-}

import Color.Convert exposing (colorToCssRgb)
import Html
import Svg exposing (Svg, circle, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, viewBox, width)
import Visualization.Force as Force
import Visualization.Scale as Scale


screenWidth : Float
screenWidth =
    990


screenHeight : Float
screenHeight =
    504


color : Int -> String
color =
    colorToCssRgb << Scale.convert (Scale.sequential ( 0, 360 ) Scale.viridisInterpolator) << toFloat


makePetal : Int -> Svg msg
makePetal i =
    let
        { x, y } =
            Force.entity i ()

        angle =
            modBy 360 (floor (toFloat i * (3 - sqrt 5) * pi * 180 - sqrt (toFloat i) * 4))
    in
    circle [ cx (toString x), cy (toString y), r "5", fill (color angle) ] []


view : List Int -> Svg msg
view model =
    svg [ width (toString screenWidth ++ "px"), height (toString screenHeight ++ "px"), viewBox "-500 -500 1000 1000" ] <|
        List.map makePetal model


main =
    view <| List.range 1 10000
