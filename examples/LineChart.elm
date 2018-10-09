module LineChart exposing (main)

{-| This module shows how to build a simple line and area chart using some of
the primitives provided in this library.
-}

import Axis
import Color
import Path exposing (Path)
import SampleData exposing (timeSeries)
import Scale exposing (ContinuousScale)
import Shape
import Time
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes exposing (class, fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (strokeWidth)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Transform(..))


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


xScale : ContinuousScale Time.Posix
xScale =
    Scale.time Time.utc ( 0, w - 2 * padding ) ( Time.millisToPosix 1448928000000, Time.millisToPosix 1456790400000 )


yScale : ContinuousScale Float
yScale =
    Scale.linear ( h - 2 * padding, 0 ) ( 0, 5 )


xAxis : List ( Time.Posix, Float ) -> Svg msg
xAxis model =
    Axis.bottom [ Axis.tickCount (List.length model) ] xScale


yAxis : Svg msg
yAxis =
    Axis.left [ Axis.tickCount 5 ] yScale


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
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis model ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ]
            [ Path.element (area model) [ strokeWidth 3, fill <| Fill <| Color.rgba 1 0 0 0.54 ]
            , Path.element (line model) [ stroke (Color.rgb 1 0 0), strokeWidth 3, fill FillNone ]
            ]
        ]



-- From here onwards this is simply example boilerplate.
-- In a real app you would load the data from a server and parse it, perhaps in
-- a separate module.


main =
    view timeSeries
