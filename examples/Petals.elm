module Petals exposing (main)

import Html
import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)
import Visualization.Force as Force


screenWidth : Float
screenWidth =
    990


screenHeight : Float
screenHeight =
    504


makePetal : Int -> Svg msg
makePetal i =
    let
        { x, y } =
            Force.entity i i
    in
        circle [ cx (toString x), cy (toString y), r "3" ] []


view : List Int -> Svg msg
view model =
    svg [ width (toString screenWidth ++ "px"), height (toString screenHeight ++ "px"), viewBox "-500 -500 1000 1000" ] <|
        List.map makePetal model


main =
    view <| List.range 1 100
