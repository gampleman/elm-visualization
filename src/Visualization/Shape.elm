module Visualization.Shape exposing (line, area, linearCurve, monotoneInXCurve, Curve)

{-| Visualizations typically consist of discrete graphical marks, such as symbols,
arcs, lines and areas. While the rectangles of a bar chart may be easy enough to
generate directly using SVG or Canvas, other shapes are complex, such as rounded
annular sectors and centripetal Catmull–Rom splines. This module provides a
variety of shape generators for your convenience.

**Note:** Currently only rudimentary forms are implemented.

# Lines

@docs line, area

# Curves

@docs linearCurve, monotoneInXCurve, Curve

-}

import Visualization.Path as Path exposing (..)


type alias Point =
    ( Float, Float )


{-| A curve is represented as a list of points, which a curve function can turn
into drawing commands.
-}
type Curve
    = Line (List Point)
    | Area (List ( Point, Point ))


applyRecursivelyForArea : (Curve -> List PathSegment) -> List ( Point, Point ) -> List PathSegment
applyRecursivelyForArea fn points =
    let
        points0 =
            List.map fst points

        points1 =
            List.reverse <| List.map snd points
    in
        case ( fn (Line points1), points1 ) of
            ( _ :: tail, ( x, y ) :: _ ) ->
                (fn (Line points0) |> Path.lineTo x y) ++ (tail |> Path.close)

            _ ->
                []


{-| Produces a polyline through the specified points.
-}
linearCurve : Curve -> List PathSegment
linearCurve part =
    case part of
        Line [] ->
            []

        Line (point :: points) ->
            Path.Move point :: List.map Path.Line points

        Area points ->
            applyRecursivelyForArea linearCurve points


{-| Produces a cubic spline that [preserves monotonicity](http://adsabs.harvard.edu/full/1990A%26A...239..443S)
in y, assuming monotonicity in x, as proposed by Steffen in
[A simple method for monotonic interpolation in one dimension](http://adsabs.harvard.edu/full/1990A%26A...239..443S):
“a smooth curve with continuous first-order derivatives that passes through any
given set of data points without spurious oscillations. Local extrema can occur
only at grid points where they are given by the data, but not in between two adjacent grid points.”
-}
monotoneInXCurve : Curve -> List PathSegment
monotoneInXCurve part =
    let
        point ( x0, y0 ) ( x1, y1 ) t0 t1 path =
            let
                dx =
                    (x1 - x0) / 3
            in
                path |> Path.bezierCurveTo (x0 + dx) (y0 + dx * t0) (x1 - dx) (y1 - dx * t1) x1 y1

        finalize ( ( x0, y0 ), ( x1, y1 ), t0maybe, path ) =
            case t0maybe of
                Nothing ->
                    path |> Path.lineTo x1 y1

                Just t0 ->
                    point ( x0, y0 ) ( x1, y1 ) t0 (slope2 ( x0, y0 ) ( x1, y1 ) t0) path

        helper ( x, y ) ( ( x0, y0 ), ( x1, y1 ), t0maybe, path ) =
            let
                t1 =
                    slope3 ( x0, y0 ) ( x1, y1 ) ( x, y )

                t0 =
                    case t0maybe of
                        Nothing ->
                            slope2 ( x0, y0 ) ( x1, y1 ) t1

                        Just t0' ->
                            t0'
            in
                ( ( x1, y1 ), ( x, y ), Just t1, point ( x0, y0 ) ( x1, y1 ) t0 t1 path )
    in
        case part of
            Line [] ->
                []

            Line (_ :: []) ->
                []

            Line (point0 :: point1 :: points) ->
                finalize <| List.foldl helper ( point0, point1, Nothing, [ Path.Move point0 ] ) points

            Area points ->
                applyRecursivelyForArea monotoneInXCurve points


sign : Float -> Float
sign x =
    if x < 0 then
        -1
    else
        1



{- Calculate a one-sided slope. -}


slope2 : ( Float, Float ) -> ( Float, Float ) -> Float -> Float
slope2 ( x0, y0 ) ( x1, y1 ) t =
    let
        h =
            x1 - x0
    in
        if h == 0 then
            t
        else
            3 * (y1 - y0) / h - t / 2



{- Calculate the slopes of the tangents. -}


slope3 : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Float
slope3 ( x0, y0 ) ( x1, y1 ) ( x2, y2 ) =
    let
        h0 =
            x1 - x0

        h1 =
            x2 - x1

        h =
            if h1 == 0 then
                if h0 < 0 then
                    0
                else
                    h0
            else
                h1

        s0 =
            (y1 - y0) / h

        s1 =
            (y2 - y1) / h

        p =
            (s0 * h1 + s1 * h0) / (h0 + h1)
    in
        (sign s0 + sign s1) * min (min (abs s0) (abs s1)) (0.5 * abs p)


{-| Generates a line for the given array of points which can be passed to the `d`
attribute of the `path` SVG element. It needs to be suplied with a curve function.
Points accepted are `Maybe`s, Nothing represent gaps in the data and corresponding
gaps will be rendered in the line.

**Note:** A single point (surrounded by Nothing) may not be visible.

Usually you will need to convert your data into a format supported by this function.
For example, if your data is a `List (Date, Float)`, you might use something like:

    lineGenerator : ( Date, Float ) -> Maybe ( Float, Float )
    lineGenerator ( x, y ) =
        Just ( Scale.convert xScale x, Scale.convert yScale y )

    linePath : List (Date, Float) -> String
    linePath data =
        List.map lineGenerator data
            |> Shape.line Shape.linearCurve

where `xScale` and `yScale` would be appropriate `Scale`s.
-}
line : (Curve -> List PathSegment) -> List (Maybe Point) -> String
line curve data =
    let
        makeCurves datum ( prev, list ) =
            case ( prev, datum, list ) of
                ( _, Nothing, l ) ->
                    ( Nothing, l )

                ( Nothing, Just point, l ) ->
                    ( Just point, Line [ point ] :: l )

                ( Just p0, Just p1, (Line ps) :: l ) ->
                    ( Just p1, Line (p1 :: ps) :: l )

                ( Just p0, Just p1, l ) ->
                    ( Just p1, Line [ p1 ] :: l )
    in
        toAttrString <| List.concatMap curve <| snd <| List.foldr makeCurves ( Nothing, [] ) data


{-| The area generator produces an area, as in an area chart. An area is defined
by two bounding lines, either splines or polylines. Typically, the two lines
share the same x-values (x0 = x1), differing only in y-value (y0 and y1);
most commonly, y0 is defined as a constant representing zero. The first line
(the topline) is defined by x1 and y1 and is rendered first; the second line
(the baseline) is defined by x0 and y0 and is rendered second, with the points
in reverse order. With a `linearCurve` curve, this produces a clockwise polygon.

The data attribute you pass in should be a `[Just ((x0, y0), (x1, y1))]`. Passing
in `Nothing` represents gaps in the data and corresponding gaps in the area will
be rendered.

Usually you will need to convert your data into a format supported by this function.
For example, if your data is a `List (Date, Float)`, you might use something like:

    areaGenerator : ( Date, Float ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
    areaGenerator ( x, y ) =
        Just ( ( Scale.convert xScale x, fst (Scale.rangeExtent yScale) ),
               ( Scale.convert xScale x, Scale.convert yScale y ) )

    areaPath : List (Date, Float) -> String
    areaPath data =
        List.map areaGenerator data
            |> Shape.area Shape.linearCurve

where `xScale` and `yScale` would be appropriate `Scale`s.
-}
area : (Curve -> List PathSegment) -> List (Maybe ( Point, Point )) -> String
area curve data =
    let
        makeCurves datum ( prev, list ) =
            case ( prev, datum, list ) of
                ( _, Nothing, l ) ->
                    ( Nothing, l )

                ( Nothing, Just pair, l ) ->
                    ( Just pair, Area [ pair ] :: l )

                ( Just p0, Just p1, (Area ps) :: l ) ->
                    ( Just p1, Area (p1 :: ps) :: l )

                ( Just p0, Just p1, l ) ->
                    ( Just p1, Area [ p1 ] :: l )
    in
        toAttrString <| List.concatMap curve <| snd <| List.foldr makeCurves ( Nothing, [] ) data
