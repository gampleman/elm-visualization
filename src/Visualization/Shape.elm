module Visualization.Shape exposing (line, area, linearCurve, monotoneInXCurve, Curve, pie, Arc, arc, centroid, customPie)

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
import Array exposing (Array)
import Dict exposing (Dict)


type alias Arc =
    { innerRadius : Float
    , outerRadius : Float
    , cornerRadius : Float
    , startAngle : Float
    , endAngle : Float
    , padAngle : Float
    , padRadius : Float
    }


epsilon =
    1.0e-12


intersect : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> ( Float, Float )
intersect x0 y0 x1 y1 x2 y2 x3 y3 =
    let
        x10 =
            x1 - x0

        y10 =
            y1 - y0

        x32 =
            x3 - x2

        y32 =
            y3 - y2

        t =
            (x32 * (y0 - y2) - y32 * (x0 - x2)) / (y32 * x10 - x32 * y10)
    in
        ( x0 + t * x10, y0 + t * y10 )


cornerTangents : Float -> Float -> Float -> Float -> Float -> Float -> Bool -> { cx : Float, cy : Float, x01 : Float, y01 : Float, x11 : Float, y11 : Float }
cornerTangents x0 y0 x1 y1 r1 rc cw =
    let
        x01 =
            x0 - x1

        y01 =
            y0 - y1

        lo =
            (if cw then
                rc
             else
                -rc
            )
                / sqrt (x01 ^ 2 + y01 ^ 2)

        ox =
            lo * y01

        oy =
            -lo * x01

        x11 =
            x0 + ox

        y11 =
            y0 + oy

        x10 =
            x1 + ox

        y10 =
            y1 + oy

        x00 =
            (x11 + x10) / 2

        y00 =
            (y11 + y10) / 2

        dx =
            x10 - x11

        dy =
            y10 - y11

        d2 =
            dx ^ 2 + dy ^ 2

        r =
            r1 - rc

        dd =
            x11 * y10 - x10 * y11

        d =
            (if dy < 0 then
                -1
             else
                1
            )
                * sqrt (max 0 (r ^ 2 * d2 - dd ^ 2))

        cx0 =
            (dd * dy - dx * d) / d2

        cy0 =
            (-dd * dx - dy * d) / d2

        cx1 =
            (dd * dy + dx * d) / d2

        cy1 =
            (-dd * dx + dy * d) / d2

        dx0 =
            cx0 - x00

        dy0 =
            cy0 - y00

        dx1 =
            cx1 - x00

        dy1 =
            cy1 - y00

        ( fcx, fxy ) =
            if dx0 ^ 2 + dy0 ^ 2 > dx1 ^ 2 + dy1 ^ 2 then
                ( cx1, cy1 )
            else
                ( cx0, cy0 )
    in
        { cx = fcx
        , cy = fxy
        , x01 = -ox
        , y01 = -oy
        , x11 = fcx * (r1 / r - 1)
        , y11 = fxy * (r1 / r - 1)
        }


myAsin : Float -> Float
myAsin x =
    if x >= 1 then
        pi / 2
    else if x <= -1 then
        -pi / 2
    else
        asin x


arc : Arc -> String
arc arcData =
    let
        ( r0, r1 ) =
            if arcData.innerRadius > arcData.outerRadius then
                ( arcData.outerRadius, arcData.innerRadius )
            else
                ( arcData.innerRadius, arcData.outerRadius )

        a0 =
            arcData.startAngle - pi / 2

        a1 =
            arcData.endAngle - pi / 2

        da =
            abs (a1 - a0)

        cw =
            a1 > a0

        path =
            -- Is it a point?
            if r1 <= epsilon then
                Path.begin |> moveTo 0 0
                -- Or is it a circle or annulus?
            else if da > 2 * pi - epsilon then
                let
                    p =
                        Path.begin
                            |> moveTo (r1 * cos a0) (r1 * sin a0)
                            |> Path.arc 0 0 r1 a0 a1 (not cw)
                in
                    if r0 > epsilon then
                        p
                            |> moveTo (r0 * cos a1) (r0 * sin a1)
                            |> Path.arc 0 0 r0 a1 a0 cw
                    else
                        p
                -- Or is it a circular or annular sector?
            else
                let
                    ap =
                        arcData.padAngle / 2

                    rp =
                        if ap > epsilon then
                            if arcData.padRadius > 0 then
                                arcData.padRadius
                            else
                                sqrt (r0 ^ 2 + r1 ^ 2)
                        else
                            0

                    p0 =
                        myAsin (rp / r0 * sin ap)

                    p1 =
                        myAsin (rp / r1 * sin ap)

                    -- Apply padding? Note that since r1 ≥ r0, da1 ≥ da0.
                    ( a00, a10, da0 ) =
                        if rp > epsilon then
                            if da - p0 * 2 > epsilon then
                                if cw then
                                    ( a0 + p0, a1 - p0, da - p0 * 2 )
                                else
                                    ( a0 - p0, a1 + p0, da - p0 * 2 )
                            else
                                ( (a0 + a1) / 2, (a0 + a1) / 2, 0 )
                        else
                            ( a0, a1, da )

                    ( a01, a11, da1 ) =
                        if rp > epsilon then
                            if da - p1 * 2 > epsilon then
                                if cw then
                                    ( a0 + p1, a1 - p1, da - p1 * 2 )
                                else
                                    ( a0 - p1, a1 + p1, da - p1 * 2 )
                            else
                                ( (a0 + a1) / 2, (a0 + a1) / 2, 0 )
                        else
                            ( a0, a1, da )

                    rc =
                        min (abs (r1 - r0) / 2) arcData.cornerRadius

                    x01 =
                        r1 * cos a01

                    y01 =
                        r1 * sin a01

                    x10 =
                        r0 * cos a10

                    y10 =
                        r0 * sin a10

                    x11 =
                        r1 * cos a11

                    y11 =
                        r1 * sin a11

                    x00 =
                        r0 * cos a00

                    y00 =
                        r0 * sin a00

                    ( ocx, ocy ) =
                        if da0 > epsilon then
                            intersect x01 y01 x00 y00 x11 y11 x10 y10
                        else
                            ( x10, y10 )

                    ( ax, ay, bx, by ) =
                        ( x01 - ocx, y01 - ocy, x11 - ocx, y11 - ocy )

                    kc =
                        1 / sin (acos ((ax * bx + ay * by) / (sqrt (ax ^ 2 + ay ^ 2) * sqrt (bx ^ 2 + by ^ 2))) / 2)

                    lc =
                        sqrt (ocx ^ 2 + ocy ^ 2)

                    ( rc0, rc1 ) =
                        if rc > epsilon && da < pi then
                            ( min rc ((r0 - lc) / (kc - 1)), min rc ((r1 - lc) / (kc + 1)) )
                        else
                            ( rc, rc )

                    outerRing =
                        -- Is the sector collapsed to a line?
                        if da1 <= epsilon then
                            Path.begin |> Path.moveTo x01 y01
                            -- Does the sector’s outer ring have rounded corners?
                        else if rc1 > epsilon then
                            let
                                t0 =
                                    cornerTangents x00 y00 x01 y01 r1 rc1 cw

                                t1 =
                                    cornerTangents x11 y11 x10 y10 r1 rc1 cw

                                p =
                                    Path.begin
                                        |> Path.moveTo (t0.cx + t0.x01) (t0.cy + t0.y01)
                            in
                                -- Have the corners merged?
                                if rc1 < rc then
                                    p |> Path.arc t0.cx t0.cy rc1 (atan2 t0.y01 t0.x01) (atan2 t1.y01 t1.x01) (not cw)
                                    -- Otherwise, draw the two corners and the ring.
                                else
                                    p
                                        |> Path.arc t0.cx t0.cy rc1 (atan2 t0.y01 t0.x01) (atan2 t0.y11 t0.x11) (not cw)
                                        |> Path.arc 0 0 r1 (atan2 (t0.cy + t0.y11) (t0.cx + t0.x11)) (atan2 (t1.cy + t1.y11) (t1.cx + t1.x11)) (not cw)
                                        |> Path.arc t1.cx t1.cy rc1 (atan2 t1.y11 t1.x11) (atan2 t1.y01 t1.x01) (not cw)
                            -- Or is the outer ring just a circular arc?
                        else
                            Path.begin
                                |> Path.moveTo x01 y01
                                |> Path.arc 0 0 r1 a01 a11 (not cw)
                in
                    -- Is there no inner ring, and it’s a circular sector?
                    -- Or perhaps it’s an annular sector collapsed due to padding?
                    if r0 <= epsilon || da0 <= epsilon then
                        outerRing
                            |> lineTo x10 y10
                        -- Does the sector’s inner ring (or point) have rounded corners?
                    else if rc0 > epsilon then
                        let
                            t0 =
                                cornerTangents x10 y10 x11 y11 r0 -rc0 cw

                            t1 =
                                cornerTangents x01 y01 x00 y00 r0 -rc0 cw

                            p =
                                outerRing
                                    |> Path.lineTo (t0.cx + t0.x01) (t0.cy + t0.y01)
                        in
                            --Have the corners merged?
                            if rc0 < rc then
                                p |> Path.arc t0.cx t0.cy rc0 (atan2 t0.y01 t0.x01) (atan2 t1.y01 t1.x01) (not cw)
                            else
                                p
                                    |> Path.arc t0.cx t0.cy rc0 (atan2 t0.y01 t0.x01) (atan2 t0.y11 t0.x11) (not cw)
                                    |> Path.arc 0 0 r0 (atan2 (t0.cy + t0.y11) (t0.cx + t0.x11)) (atan2 (t1.cy + t1.y11) (t1.cx + t1.x11)) cw
                                    |> Path.arc t1.cx t1.cy rc0 (atan2 t1.y11 t1.x11) (atan2 t1.y01 t1.x01) (not cw)
                        -- Or is the inner ring just a circular arc?
                    else
                        outerRing
                            |> Path.arc 0 0 r0 a10 a00 cw
    in
        path |> close |> toAttrString


centroid : Arc -> Point
centroid arcData =
    let
        r =
            (arcData.innerRadius + arcData.outerRadius) / 2

        a =
            (arcData.startAngle + arcData.endAngle) / 2 - pi / 2
    in
        ( cos a * r, sin a * r )


pie : Float -> Float -> Float -> List Float -> List Arc
pie inner outer padding =
    customPie
        { startAngle = 0
        , endAngle = 2 * pi
        , padAngle = padding
        , sortingFn = Basics.compare
        , valueFn = identity
        , innerRadius = always inner
        , outerRadius = always outer
        , cornerRadius = always 0
        , padRadius = 0
        }


customPie :
    { startAngle : Float
    , endAngle : Float
    , padAngle : Float
    , sortingFn : a -> a -> Order
    , valueFn : a -> Float
    , innerRadius : a -> Float
    , outerRadius : a -> Float
    , cornerRadius : a -> Float
    , padRadius : Float
    }
    -> List a
    -> List Arc
customPie settings data =
    let
        sum =
            List.foldr (\a b -> settings.valueFn a + b) 0 data

        da =
            min (2 * pi) (max (-2 * pi) (settings.endAngle - settings.startAngle))

        p =
            min (abs da / toFloat (List.length data)) (settings.padAngle)

        pa =
            p
                * (if da < 0 then
                    -1
                   else
                    1
                  )

        sortedIndices =
            List.map fst << List.sortWith (\a b -> settings.sortingFn (snd a) (snd b)) << List.indexedMap (,)

        dataArray =
            Array.fromList data

        k =
            if sum > 0 then
                (da - toFloat (List.length data) * pa) / sum
            else
                0

        computeValue el angle =
            let
                value =
                    settings.valueFn el
            in
                { innerRadius = settings.innerRadius el
                , outerRadius = settings.outerRadius el
                , cornerRadius = settings.cornerRadius el
                , startAngle = angle
                , endAngle =
                    angle
                        + (if value > 0 then
                            value * k
                           else
                            0
                          )
                        + pa
                , padAngle = p
                , padRadius = settings.padRadius
                }

        unsafeGet index array =
            case Array.get index array of
                Just v ->
                    v

                Nothing ->
                    Debug.crash "This should never happen"

        helper index ( angle, result ) =
            let
                r =
                    computeValue (unsafeGet index dataArray) angle
            in
                ( r.endAngle, Dict.insert index r result )
    in
        sortedIndices data
            |> List.foldr helper ( settings.startAngle, Dict.empty )
            |> snd
            |> Dict.values


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
