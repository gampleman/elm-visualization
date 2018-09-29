module CornerRadius exposing (main)

{-| A demonstration of cornerRadius for arcs
-}

import Array exposing (Array)
import Path exposing (Path)
import Svg.Attributes exposing (fill, stroke)
import TypedSvg exposing (circle, g, svg)
import TypedSvg.Attributes exposing (transform)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Transform(..))
import Visualization.Shape as Shape exposing (Arc, defaultPieConfig)


w : Float
w =
    990


h : Float
h =
    504


cornerRadius : Float
cornerRadius =
    12


colors : Array String
colors =
    Array.fromList
        [ "rgba(31, 119, 180, 0.5)"
        , "rgba(255, 127, 14, 0.5)"
        , "rgba(44, 159, 44, 0.5)"
        , "rgba(214, 39, 40, 0.5)"
        , "rgba(148, 103, 189, 0.5)"
        , "rgba(140, 86, 75, 0.5)"
        , "rgba(227, 119, 194, 0.5)"
        , "rgba(128, 128, 128, 0.5)"
        , "rgba(188, 189, 34, 0.5)"
        , "rgba(23, 190, 207, 0.5)"
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
        , stroke "#000"
        , fill "none"
        ]


circular : List Arc -> Svg msg
circular arcs =
    let
        makeSlice index datum =
            Path.element (Shape.arc datum) [ fill (Maybe.withDefault "#000" <| Array.get index colors), stroke "#fff" ]

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
            Path.element (Shape.arc { datum | innerRadius = mainRadius - 60 }) [ fill (Maybe.withDefault "#000" <| Array.get index colors), stroke "#fff" ]

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
    svg [ width w, height h ]
        [ circular pieData
        , annular pieData
        ]


data : List Float
data =
    [ 1, 1, 2, 3, 5, 8, 13 ]


main : Svg msg
main =
    view data
