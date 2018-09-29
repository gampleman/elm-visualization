module LineChart exposing (main)

{-| This module shows how to build a simple line and area chart using some of
the primitives provided in this library.
-}

import Path exposing (Path)
import SampleData exposing (timeSeries)
import Svg.Attributes exposing (stroke)
import Time
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes exposing (class, fill, transform)
import TypedSvg.Attributes.InPx exposing (height, strokeWidth, width)
import TypedSvg.Color exposing (rgba)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Transform(..))
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.List as List
import Visualization.Scale as Scale exposing (ContinuousScale, ContinuousTimeScale)
import Visualization.Shape as Shape


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
    Scale.time Time.utc ( Time.millisToPosix 1448928000000, Time.millisToPosix 1456790400000 ) ( 0, w - 2 * padding )


yScale : ContinuousScale
yScale =
    Scale.linear ( 0, 5 ) ( h - 2 * padding, 0 )


xAxis : List ( Time.Posix, Float ) -> Svg msg
xAxis model =
    Axis.axis { defaultOptions | orientation = Axis.Bottom, tickCount = List.length model } xScale


yAxis : Svg msg
yAxis =
    Axis.axis { defaultOptions | orientation = Axis.Left, tickCount = 5 } yScale


transformToLineData : ( Time.Posix, Float ) -> Maybe ( Float, Float )
transformToLineData ( x, y ) =
    Just ( Scale.convert xScale x, Scale.convert yScale y )


tranfromToAreaData : ( Time.Posix, Float ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
tranfromToAreaData ( x, y ) =
    Just
        ( ( Scale.convert xScale x, Tuple.first (Scale.rangeExtent yScale) )
        , ( Scale.convert xScale x, Scale.convert yScale y )
        )


line : List ( Time.Posix, Float ) -> Path
line model =
    List.map transformToLineData model
        |> Shape.line Shape.monotoneInXCurve


area : List ( Time.Posix, Float ) -> Path
area model =
    List.map tranfromToAreaData model
        |> Shape.area Shape.monotoneInXCurve


view : List ( Time.Posix, Float ) -> Svg msg
view model =
    svg [ width w, height h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis model ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ]
            [ Path.element (area model) [ stroke "none", strokeWidth 3, fill <| Fill <| rgba 255 0 0 0.54 ]
            , Path.element (line model) [ stroke "red", strokeWidth 3, fill FillNone ]
            ]
        ]



-- From here onwards this is simply example boilerplate.
-- In a real app you would load the data from a server and parse it, perhaps in
-- a separate module.


main =
    view timeSeries
