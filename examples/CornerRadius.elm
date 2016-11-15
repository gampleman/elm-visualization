module CornerRadius exposing (main)

{-| A demonstration of cornerRadius for arcs
-}

import Visualization.Shape as Shape exposing (defaultPieConfig, Arc)
import Array exposing (Array)
import Svg exposing (Svg, svg, g, path, text)
import Svg.Attributes exposing (transform, d, style, dy, width, height, textAnchor)


screenWidth : Float
screenWidth =
    990


screenHeight : Float
screenHeight =
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


radius : Float
radius =
    min (screenWidth / 2) screenHeight / 2 - 10


circle : String
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
    path
        [ transform
            ("translate"
                ++ toString
                    ( sign * cornerRadius * cos angle + sqrt (radius ^ 2 - cornerRadius ^ 2) * sin angle
                    , sign * cornerRadius * sin angle - sqrt (radius ^ 2 - cornerRadius ^ 2) * cos angle
                    )
            )
        , style "stroke: #000; fill: none"
        , d circle
        ]
        []


circular : List Arc -> Svg msg
circular arcs =
    let
        makeSlice index datum =
            path [ d (Shape.arc datum), style ("fill:" ++ (Maybe.withDefault "#000" <| Array.get index colors) ++ "; stroke: #fff;") ] []

        makeCorners : { a | startAngle : Float, endAngle : Float, outerRadius : Float } -> List (Svg msg)
        makeCorners { startAngle, endAngle, outerRadius } =
            [ corner startAngle (outerRadius - cornerRadius) 1
            , corner endAngle (outerRadius - cornerRadius) -1
            ]
    in
        g [ transform ("translate(" ++ toString radius ++ "," ++ toString radius ++ ")") ]
            [ g [] <| List.indexedMap makeSlice arcs
            , g [] <| List.concatMap makeCorners arcs
            ]


annular : List Arc -> Svg msg
annular arcs =
    let
        makeSlice index datum =
            path [ d (Shape.arc { datum | innerRadius = radius - 60 }), style ("fill:" ++ (Maybe.withDefault "#000" <| Array.get index colors) ++ "; stroke: #fff;") ] []

        makeCorners { startAngle, endAngle, outerRadius, innerRadius } =
            [ corner startAngle (outerRadius - cornerRadius) 1
            , corner endAngle (outerRadius - cornerRadius) -1
            , corner endAngle (radius - 60 + cornerRadius) -1
            , corner startAngle (radius - 60 + cornerRadius) 1
            ]
    in
        g [ transform ("translate(" ++ toString (3 * radius + 20) ++ "," ++ toString radius ++ ")") ]
            [ g [] <| List.indexedMap makeSlice arcs
            , g [] <| List.concatMap makeCorners arcs
            ]


view : List Float -> Svg msg
view model =
    let
        pieData =
            model |> Shape.pie { defaultPieConfig | outerRadius = radius, cornerRadius = cornerRadius }
    in
        svg [ width (toString screenWidth ++ "px"), height (toString screenHeight ++ "px") ]
            [ circular pieData
            , annular pieData
            ]


model : List Float
model =
    [ 1, 1, 2, 3, 5, 8, 13 ]


main : Svg msg
main =
    view model
