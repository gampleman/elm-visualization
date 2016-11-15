module Curves exposing (main)

{-| Here we demonstrate the various curve functions provided.
-}

import Html exposing (div, p, a)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Visualization.Scale as Scale exposing (ContinuousScale)
import Visualization.Shape as Shape
import Svg exposing (Svg, svg, rect, path, g, line, text, text_)
import Svg.Attributes exposing (..)
import Visualization.Path


screenWidth : Float
screenWidth =
    990


screenHeight : Float
screenHeight =
    450


padding : Float
padding =
    50


points : List ( Float, Float )
points =
    [ ( 0.1, 0.1 )
    , ( 0.2, 0.6 )
    , ( 0.35, 0.9 )
    , ( 0.45, 0.9 )
    , ( 0.6, 0.3 )
    , ( 0.9, 0.8 )
    , ( 1.2, 0.6 )
    , ( 1.5, 0.4 )
    , ( 1.7, 0.2 )
    , ( 1.9, 0.7 )
    ]


xScale : ContinuousScale
xScale =
    Scale.linear ( 0, 2 ) ( padding, screenWidth - padding )


yScale : ContinuousScale
yScale =
    Scale.linear ( 0, 1 ) ( screenHeight - padding, padding )


preparedPoints : List ( Float, Float )
preparedPoints =
    List.map (\( x, y ) -> ( Scale.convert xScale x, Scale.convert yScale y )) points


xGridLine : Int -> Float -> Svg msg
xGridLine index tick =
    line
        [ y1 "0"
        , y2 "100%"
        , x1 (toString (Scale.convert xScale tick))
        , x2 (toString (Scale.convert xScale tick))
        , stroke "white"
        , strokeWidth (toString (Basics.max (toFloat (index % 2)) 0.5))
        ]
        []


yGridLine : Int -> Float -> Svg msg
yGridLine index tick =
    line
        [ x1 "0"
        , x2 "100%"
        , y1 (toString (Scale.convert yScale tick))
        , y2 (toString (Scale.convert yScale tick))
        , stroke "white"
        , strokeWidth (toString (Basics.max (toFloat (index % 2)) 0.5))
        ]
        []


type alias Curve =
    Shape.Curve -> List Visualization.Path.PathSegment


drawCurve : ( String, Curve, String ) -> Svg msg
drawCurve ( name, curve, color ) =
    Svg.path [ d (Shape.line curve (List.map Just preparedPoints)), stroke color, fill "none", strokeWidth "2" ] []


drawLegend : Int -> ( String, Curve, String ) -> Svg msg
drawLegend index ( name, curve, color ) =
    text_ [ style ("color: " ++ color ++ "; font-family: monospace"), x (toString padding), y (toString (toFloat index * 20 + padding)) ] [ text name ]


view : Msg -> Svg Msg
view model =
    div []
        [ p []
            [ text "Curve type: "
            , a [ href "#linear", onClick Linear ] [ text "Linear" ]
            , text " "
            , a [ href "#monotone-in-x", onClick MonotoneInX ] [ text "MonotoneInX" ]
            ]
        , svg [ width (toString screenWidth), height (toString screenHeight) ]
            [ rect [ width "100%", height "100%", fill "#dfdfdf" ] []
            , g [] <| List.indexedMap yGridLine <| Scale.ticks yScale 10
            , g [] <| List.indexedMap xGridLine <| Scale.ticks xScale 20
            , g [] <|
                List.map drawCurve (curveTypes model)
            , g [] <| List.map (\point -> Svg.path [ d circle, fill "white", stroke "black", transform ("translate" ++ toString point) ] []) preparedPoints
            , g [] <| List.indexedMap drawLegend <| curveTypes model
            ]
        ]


curveTypes : Msg -> List ( String, Curve, String )
curveTypes model =
    case model of
        Linear ->
            [ ( "linearCurve", Shape.linearCurve, "#000" ) ]

        MonotoneInX ->
            [ ( "monotoneInXCurve", Shape.monotoneInXCurve, "#000" ) ]


circle : String
circle =
    Shape.arc
        { innerRadius = 0
        , outerRadius = 3
        , cornerRadius = 0
        , startAngle = 0
        , endAngle = 2 * pi
        , padAngle = 0
        , padRadius = 0
        }


type Msg
    = Linear
    | MonotoneInX


main : Program Never Msg Msg
main =
    Html.beginnerProgram { model = Linear, view = view, update = (\msg model -> msg) }
