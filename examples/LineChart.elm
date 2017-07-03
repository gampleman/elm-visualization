module LineChart exposing (main)

{-| This module shows how to build a simple line and area chart using some of
the primitives provided in this library.
-}

import Date exposing (Date)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.List as List
import Visualization.Scale as Scale exposing (ContinuousScale, ContinuousTimeScale)
import Visualization.Shape as Shape
import SampleData exposing (timeSeries)


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


xScale : ContinuousTimeScale
xScale =
    Scale.time ( Date.fromTime 1448928000000, Date.fromTime 1456790400000 ) ( 0, w - 2 * padding )


yScale : ContinuousScale
yScale =
    Scale.linear ( 0, 5 ) ( h - 2 * padding, 0 )


xAxis : List ( Date, Float ) -> Svg msg
xAxis model =
    Axis.axis { defaultOptions | orientation = Axis.Bottom, tickCount = List.length model } xScale


yAxis : Svg msg
yAxis =
    Axis.axis { defaultOptions | orientation = Axis.Left, tickCount = 5 } yScale


transformToLineData : ( Date, Float ) -> Maybe ( Float, Float )
transformToLineData ( x, y ) =
    Just ( Scale.convert xScale x, Scale.convert yScale y )


tranfromToAreaData : ( Date, Float ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
tranfromToAreaData ( x, y ) =
    Just
        ( ( Scale.convert xScale x, Tuple.first (Scale.rangeExtent yScale) )
        , ( Scale.convert xScale x, Scale.convert yScale y )
        )


line : List ( Date, Float ) -> Attribute msg
line model =
    List.map transformToLineData model
        |> Shape.line Shape.monotoneInXCurve
        |> d


area : List ( Date, Float ) -> Attribute msg
area model =
    List.map tranfromToAreaData model
        |> Shape.area Shape.monotoneInXCurve
        |> d


view : List ( Date, Float ) -> Svg msg
view model =
    svg [ width (toString w ++ "px"), height (toString h ++ "px") ]
        [ g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (h - padding) ++ ")") ]
            [ xAxis model ]
        , g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString padding ++ ")") ]
            [ yAxis ]
        , g [ transform ("translate(" ++ toString padding ++ ", " ++ toString padding ++ ")"), class "series" ]
            [ Svg.path [ area model, stroke "none", strokeWidth "3px", fill "rgba(255, 0, 0, 0.54)" ] []
            , Svg.path [ line model, stroke "red", strokeWidth "3px", fill "none" ] []
            ]
        ]



-- From here onwards this is simply example boilerplate.
-- In a real app you would load the data from a server and parse it, perhaps in
-- a separate module.


main =
    view timeSeries
