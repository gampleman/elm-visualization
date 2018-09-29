module Curves exposing (main)

{-| Here we demonstrate the various curve functions provided.
-}

import Color
import Example
import Html exposing (a, div, p)
import Path exposing (Path)
import SubPath exposing (SubPath)
import Svg.Attributes exposing (fill, style)
import TypedSvg exposing (g, line, rect, svg, text_)
import TypedSvg.Attributes as Explicit exposing (stroke, transform)
import TypedSvg.Attributes.InPx exposing (height, strokeWidth, width, x, x1, x2, y, y1, y2)
import TypedSvg.Color exposing (black, white)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (Transform(..), percent)
import Visualization.Scale as Scale exposing (ContinuousScale)
import Visualization.Shape as Shape


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
    , ( 0.35, 0.3 )
    , ( 0.45, 0.3 )
    , ( 0.6, 0.2 )
    , ( 0.9, 0.8 )
    , ( 1.2, 0.6 )
    , ( 1.5, 0.9 )
    , ( 1.7, 0.2 )
    , ( 1.9, 0.1 )
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
        [ y1 0
        , Explicit.y2 (percent 100)
        , x1 (Scale.convert xScale tick)
        , x2 (Scale.convert xScale tick)
        , stroke white
        , strokeWidth (Basics.max (toFloat (modBy 2 index)) 0.5)
        ]
        []


yGridLine : Int -> Float -> Svg msg
yGridLine index tick =
    line
        [ x1 0
        , Explicit.x2 (percent 100)
        , y1 (Scale.convert yScale tick)
        , y2 (Scale.convert yScale tick)
        , stroke white
        , strokeWidth (Basics.max (toFloat (modBy 2 index)) 0.5)
        ]
        []


type alias Curve =
    List ( Float, Float ) -> SubPath


drawCurve : ( String, Curve, String ) -> Svg msg
drawCurve ( name, curve, color ) =
    List.map Just preparedPoints
        |> Shape.line curve
        |> (\path -> Path.element path [ Svg.Attributes.stroke color, fill "none", strokeWidth 2 ])


drawLegend : Int -> ( String, Curve, String ) -> Svg msg
drawLegend index ( name, curve, color ) =
    text_ [ style ("fill: " ++ color ++ "; font-family: monospace"), x padding, y (toFloat index * 20 + padding) ] [ text name ]


view : List ( String, Curve, String ) -> Svg String
view model =
    div []
        [ Example.navigation "Curve type" exampleData
        , svg [ width screenWidth, height screenHeight ]
            [ rect [ width screenWidth, height screenHeight, fill "#dfdfdf" ] []
            , g [] <| List.indexedMap yGridLine <| Scale.ticks yScale 10
            , g [] <| List.indexedMap xGridLine <| Scale.ticks xScale 20
            , g [] <|
                List.map drawCurve model
            , g [] <| List.map (\( dx, dy ) -> Path.element circle [ fill "white", stroke black, transform [ Translate dx dy ] ]) preparedPoints
            , g [] <| List.indexedMap drawLegend <| model
            ]
        ]


circle : Path
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


basic : String -> Curve -> List ( String, Curve, String )
basic prefix curveFn =
    [ ( prefix, curveFn, "#000" ) ]


parametrized : String -> (Float -> Curve) -> List ( String, Curve, String )
parametrized prefix curveFn =
    let
        scale =
            Scale.sequential ( 0, 1 ) Scale.magmaInterpolator

        stops =
            [ 0, 0.25, 0.5, 0.75, 1 ]
    in
    stops
        |> List.map (\s -> ( prefix ++ " " ++ String.fromFloat s, curveFn s, Scale.convert scale s |> Color.toCssString ))


exampleData =
    [ ( "Linear", basic "linearCurve" Shape.linearCurve )
    , ( "Basis", basic "basisCurve" Shape.basisCurve )
    , ( "BasisClosed", basic "basisCurveClosed" Shape.basisCurveClosed )
    , ( "BasisOpen", basic "basisCurveOpen" Shape.basisCurveOpen )
    , ( "Bundle", parametrized "bundleCurve" Shape.bundleCurve )
    , ( "Cardinal", parametrized "cardinalCurve" Shape.cardinalCurve )
    , ( "CardinalClosed", parametrized "cardinalCurveClosed" Shape.cardinalCurveClosed )
    , ( "CardinalOpen", parametrized "cardinalCurveOpen" Shape.cardinalCurveOpen )
    , ( "CatmullRom", parametrized "catmullRomCurve" Shape.catmullRomCurve )
    , ( "CatmullRomClosed", parametrized "catmullRomCurveClosed" Shape.catmullRomCurveClosed )
    , ( "CatmullRomOpen", parametrized "catmullRomCurveOpen" Shape.catmullRomCurveOpen )
    , ( "MonotoneInX", basic "monotoneInXCurve" Shape.monotoneInXCurve )
    , ( "Step", parametrized "stepCurve" Shape.stepCurve )
    , ( "Natural", basic "naturalCurve" Shape.naturalCurve )
    ]


main =
    Example.switchableViews exampleData view



{- {"additionalShots": ["linear", "basis", "basisclosed", "basisopen", "bundle", "cardinal", "cardinalclosed", "cardinalopen", "catmullrom", "catmullromclosed", "catmullromopen", "monotoneinx", "step", "natural"], "options": {"linear": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "basis": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "basisclosed": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "basisopen": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "bundle": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "cardinal": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "cardinalclosed": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "cardinalopen": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "catmullrom": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "catmullromclosed": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "catmullromopen": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "monotoneinx": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "step": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "natural": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}}} -}
