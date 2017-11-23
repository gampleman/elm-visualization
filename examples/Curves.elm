module Curves exposing (main)

{-| Here we demonstrate the various curve functions provided.
-}

import Html exposing (div, p, a)
import Color.Convert exposing (colorToCssRgb)
import Visualization.Scale as Scale exposing (ContinuousScale)
import Visualization.Shape as Shape
import Svg exposing (Svg, svg, rect, path, g, line, text, text_)
import Svg.Attributes exposing (..)
import Visualization.Path
import Example


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
    text_ [ style ("fill: " ++ color ++ "; font-family: monospace"), x (toString padding), y (toString (toFloat index * 20 + padding)) ] [ text name ]


view : List ( String, Curve, String ) -> Svg String
view model =
    div []
        [ Example.navigation "Curve type" exampleData
        , svg [ width (toString screenWidth), height (toString screenHeight) ]
            [ rect [ width "100%", height "100%", fill "#dfdfdf" ] []
            , g [] <| List.indexedMap yGridLine <| Scale.ticks yScale 10
            , g [] <| List.indexedMap xGridLine <| Scale.ticks xScale 20
            , g [] <|
                List.map drawCurve model
            , g [] <| List.map (\point -> Svg.path [ d circle, fill "white", stroke "black", transform ("translate" ++ toString point) ] []) preparedPoints
            , g [] <| List.indexedMap drawLegend <| model
            ]
        ]


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
            |> List.map (\s -> ( prefix ++ " " ++ toString s, curveFn s, Scale.convert scale s |> colorToCssRgb ))


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


main : Program Never String String
main =
    Example.switchableViews exampleData view



{- {"additionalShots": ["linear", "basis", "basisclosed", "basisopen", "bundle", "cardinal", "cardinalclosed", "cardinalopen", "catmullrom", "catmullromclosed", "catmullromopen", "monotoneinx", "step", "natural"], "options": {"linear": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "basis": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "basisclosed": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "basisopen": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "bundle": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "cardinal": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "cardinalclosed": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "cardinalopen": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "catmullrom": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "catmullromclosed": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "catmullromopen": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "monotoneinx": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "step": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}, "natural": {"webshot":{"shotOffset":{"left":0,"top": 60, "bottom": 0, "right":0}}}}} -}
