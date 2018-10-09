module CornerRadius exposing (main)

{-| A demonstration of cornerRadius for arcs
-}

import Array exposing (Array)
import Color exposing (Color)
import Path exposing (Path)
import Shape exposing (Arc, defaultPieConfig)
import TypedSvg exposing (circle, g, svg)
import TypedSvg.Attributes exposing (fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Transform(..))


w : Float
w =
    990


h : Float
h =
    504


cornerRadius : Float
cornerRadius =
    12


rgba255 : Int -> Int -> Int -> Float -> Color
rgba255 r g b a =
    Color.fromRgba { red = toFloat r / 255, green = toFloat g / 255, blue = toFloat b / 255, alpha = a }


colors : Array Color
colors =
    Array.fromList
        [ rgba255 31 119 180 0.5
        , rgba255 255 127 14 0.5
        , rgba255 44 159 44 0.5
        , rgba255 214 39 40 0.5
        , rgba255 148 103 189 0.5
        , rgba255 140 86 75 0.5
        , rgba255 227 119 194 0.5
        , rgba255 128 128 128 0.5
        , rgba255 188 189 34 0.5
        , rgba255 23 190 207 0.5
        ]


mainRadius : Float
mainRadius =
    min (w / 2) h / 2 - 10


circle : Path
circle =
    Shape.arc
        { innerRadius = 0
        , outerRadius = cornerRadius
        , cornerRadius = 0
        , startAngle = 0
        , endAngle = 2 * pi
        , padAngle = 0
        , padRadius = 0
        }


corner : Float -> Float -> Float -> Svg msg
corner angle radius sign =
    Path.element
        circle
        [ transform
            [ Translate
                (sign * cornerRadius * cos angle + sqrt (radius ^ 2 - cornerRadius ^ 2) * sin angle)
                (sign * cornerRadius * sin angle - sqrt (radius ^ 2 - cornerRadius ^ 2) * cos angle)
            ]
        , stroke Color.black
        , fill FillNone
        ]


circular : List Arc -> Svg msg
circular arcs =
    let
        makeSlice index datum =
            Path.element (Shape.arc datum)
                [ fill <| Fill <| Maybe.withDefault Color.black <| Array.get index colors
                , stroke Color.white
                ]

        makeCorners : { a | startAngle : Float, endAngle : Float, outerRadius : Float } -> List (Svg msg)
        makeCorners { startAngle, endAngle, outerRadius } =
            [ corner startAngle (outerRadius - cornerRadius) 1
            , corner endAngle (outerRadius - cornerRadius) -1
            ]
    in
    g [ transform [ Translate mainRadius mainRadius ] ]
        [ g [] <| List.indexedMap makeSlice arcs
        , g [] <| List.concatMap makeCorners arcs
        ]


annular : List Arc -> Svg msg
annular arcs =
    let
        makeSlice index datum =
            Path.element (Shape.arc { datum | innerRadius = mainRadius - 60 })
                [ fill <| Fill <| Maybe.withDefault Color.black <| Array.get index colors
                , stroke Color.white
                ]

        makeCorners { startAngle, endAngle, outerRadius, innerRadius } =
            [ corner startAngle (outerRadius - cornerRadius) 1
            , corner endAngle (outerRadius - cornerRadius) -1
            , corner endAngle (mainRadius - 60 + cornerRadius) -1
            , corner startAngle (mainRadius - 60 + cornerRadius) 1
            ]
    in
    g [ transform [ Translate (3 * mainRadius + 20) mainRadius ] ]
        [ g [] <| List.indexedMap makeSlice arcs
        , g [] <| List.concatMap makeCorners arcs
        ]


view : List Float -> Svg msg
view model =
    let
        pieData =
            model |> Shape.pie { defaultPieConfig | outerRadius = mainRadius, cornerRadius = cornerRadius }
    in
    svg [ viewBox 0 0 w h ]
        [ circular pieData
        , annular pieData
        ]


data : List Float
data =
    [ 1, 1, 2, 3, 5, 8, 13 ]


main : Svg msg
main =
    view data
