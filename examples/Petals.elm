module Petals exposing (main)

{-| Based on the arrangement of petals in a sunflower. Demonstrates the initial layout of Force.entity.
-}

import Color exposing (Color)
import Force
import Scale
import Scale.Color
import TypedSvg exposing (circle, svg)
import TypedSvg.Attributes exposing (fill, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..))


w : Float
w =
    990


h : Float
h =
    504


color : Int -> Color
color =
    Scale.convert (Scale.sequential Scale.Color.viridisInterpolator ( 0, 360 )) << toFloat


makePetal : Int -> Svg msg
makePetal i =
    let
        { x, y } =
            Force.entity i ()

        angle =
            modBy 360 (floor (toFloat i * (3 - sqrt 5) * pi * 180 - sqrt (toFloat i) * 4))
    in
    circle [ cx x, cy y, r 5, fill (Fill (color angle)) ] []


view : List Int -> Svg msg
view model =
    svg [ viewBox -1000 -500 2000 2000 ] <|
        List.map makePetal model


main =
    view <| List.range 1 10000
