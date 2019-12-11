module Shape.Pie exposing (arc, centroid, pie)

import Array
import Dict
import LowLevel.Command exposing (..)
import Path exposing (Path)
import SubPath exposing (SubPath)


boolToDirection b =
    if b then
        counterClockwise

    else
        clockwise


boolToArc b =
    if b then
        largestArc

    else
        smallestArc


mod : Float -> Float -> Float
mod a b =
    let
        frac =
            a / b
    in
    (frac - toFloat (truncate frac)) * b


makeArc : Float -> Float -> Float -> Float -> Float -> Bool -> SubPath -> SubPath
makeArc x y radius a0 a1 ccw =
    SubPath.continue (arc_ x y radius a0 a1 ccw)


arc_ x y radius a0 a1 ccw =
    let
        r =
            abs radius

        dx =
            r * cos a0

        dy =
            r * sin a0

        x0_ =
            x + dx

        y0_ =
            y + dy

        cw =
            boolToDirection (not ccw)

        tau =
            2 * pi

        da =
            if ccw then
                a0 - a1

            else
                a1 - a0

        origin =
            moveTo ( x0_, y0_ )
    in
    if r == 0 then
        SubPath.empty

    else if da > (tau - epsilon) then
        -- Is this a complete circle? Draw two arcs to complete the circle.
        SubPath.with origin
            [ arcTo
                [ { radii = ( r, r )
                  , xAxisRotate = 0
                  , arcFlag = largestArc
                  , direction = cw
                  , target = ( x - dx, y - dy )
                  }
                ]
            , arcTo
                [ { radii = ( r, r )
                  , xAxisRotate = 0
                  , arcFlag = largestArc
                  , direction = cw
                  , target = ( x0_, y0_ )
                  }
                ]
            ]

    else
        let
            da_ =
                if da < 0 then
                    mod da tau + tau

                else
                    da
        in
        -- Otherwise, draw an arc!
        SubPath.with origin
            [ arcTo
                [ { radii = ( r, r )
                  , xAxisRotate = 0
                  , arcFlag = boolToArc (da_ >= pi)
                  , direction = cw
                  , target = ( x + r * cos a1, y + r * sin a1 )
                  }
                ]
            ]


epsilon : Float
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


arc :
    { a
        | cornerRadius : Float
        , endAngle : Float
        , innerRadius : Float
        , outerRadius : Float
        , padAngle : Float
        , startAngle : Float
        , padRadius : Float
    }
    -> Path
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
                [ SubPath.with (moveTo ( 0, 0 )) [] |> SubPath.close ]
                -- Or is it a circle or annulus?

            else if da > 2 * pi - epsilon then
                let
                    p =
                        SubPath.with (moveTo ( r1 * cos a0, r1 * sin a0 )) []
                            |> makeArc 0 0 r1 a0 a1 (not cw)
                in
                if r0 > epsilon then
                    [ p
                    , SubPath.with (moveTo ( r0 * cos a1, r0 * sin a1 )) []
                        |> makeArc 0 0 r0 a1 a0 cw
                        |> SubPath.close
                    ]

                else
                    [ p |> SubPath.close ]
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

                    ( ax, ay ) =
                        ( x01 - ocx, y01 - ocy )

                    ( bx, by ) =
                        ( x11 - ocx, y11 - ocy )

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
                            SubPath.with (moveTo ( x01, y01 )) []
                            -- Does the sector’s outer ring have rounded corners?

                        else if rc1 > epsilon then
                            let
                                t0 =
                                    cornerTangents x00 y00 x01 y01 r1 rc1 cw

                                t1 =
                                    cornerTangents x11 y11 x10 y10 r1 rc1 cw

                                p =
                                    SubPath.with (moveTo ( t0.cx + t0.x01, t0.cy + t0.y01 )) []
                            in
                            -- Have the corners merged?
                            if rc1 < rc then
                                p |> makeArc t0.cx t0.cy rc1 (atan2 t0.y01 t0.x01) (atan2 t1.y01 t1.x01) (not cw)
                                -- Otherwise, draw the two corners and the ring.

                            else
                                p
                                    |> makeArc t0.cx t0.cy rc1 (atan2 t0.y01 t0.x01) (atan2 t0.y11 t0.x11) (not cw)
                                    |> makeArc 0 0 r1 (atan2 (t0.cy + t0.y11) (t0.cx + t0.x11)) (atan2 (t1.cy + t1.y11) (t1.cx + t1.x11)) (not cw)
                                    |> makeArc t1.cx t1.cy rc1 (atan2 t1.y11 t1.x11) (atan2 t1.y01 t1.x01) (not cw)
                            -- Or is the outer ring just a circular arc?

                        else
                            SubPath.with (moveTo ( x01, y01 )) []
                                |> makeArc 0 0 r1 a01 a11 (not cw)
                in
                -- Is there no inner ring, and it’s a circular sector?
                -- Or perhaps it’s an annular sector collapsed due to padding?
                if r0 <= epsilon || da0 <= epsilon then
                    [ outerRing
                        |> SubPath.connect (SubPath.with (moveTo ( x10, y10 )) [])
                        |> SubPath.close
                    ]
                    -- Does the sector’s inner ring (or point) have rounded corners?

                else if rc0 > epsilon then
                    let
                        t0 =
                            cornerTangents x10 y10 x11 y11 r0 -rc0 cw

                        t1 =
                            cornerTangents x01 y01 x00 y00 r0 -rc0 cw

                        p =
                            outerRing
                                |> SubPath.connect (SubPath.with (moveTo ( t0.cx + t0.x01, t0.cy + t0.y01 )) [])
                    in
                    --Have the corners merged?
                    if rc0 < rc then
                        [ p |> makeArc t0.cx t0.cy rc0 (atan2 t0.y01 t0.x01) (atan2 t1.y01 t1.x01) (not cw) |> SubPath.close ]

                    else
                        [ p
                            |> makeArc t0.cx t0.cy rc0 (atan2 t0.y01 t0.x01) (atan2 t0.y11 t0.x11) (not cw)
                            |> makeArc 0 0 r0 (atan2 (t0.cy + t0.y11) (t0.cx + t0.x11)) (atan2 (t1.cy + t1.y11) (t1.cx + t1.x11)) cw
                            |> makeArc t1.cx t1.cy rc0 (atan2 t1.y11 t1.x11) (atan2 t1.y01 t1.x01) (not cw)
                            |> SubPath.close
                        ]
                    -- Or is the inner ring just a circular arc?

                else
                    [ outerRing
                        |> SubPath.connect (arc_ 0 0 r0 a10 a00 cw)
                        |> SubPath.close
                    ]
    in
    path


centroid :
    { a
        | endAngle : Float
        , innerRadius : Float
        , outerRadius : Float
        , startAngle : Float
    }
    -> ( Float, Float )
centroid arcData =
    let
        r =
            (arcData.innerRadius + arcData.outerRadius) / 2

        a =
            (arcData.startAngle + arcData.endAngle) / 2 - pi / 2
    in
    ( cos a * r, sin a * r )


pie :
    { f
        | cornerRadius : a
        , endAngle : Float
        , innerRadius : b
        , outerRadius : c
        , padAngle : Float
        , padRadius : d
        , sortingFn : e -> e -> Order
        , valueFn : e -> Float
        , startAngle : Float
    }
    -> List e
    ->
        List
            { cornerRadius : a
            , innerRadius : b
            , outerRadius : c
            , padAngle : Float
            , padRadius : d
            , startAngle : Float
            , endAngle : Float
            }
pie settings data =
    let
        summer a b =
            let
                v =
                    settings.valueFn a
            in
            if v > 0 then
                v + b

            else
                b

        sum =
            List.foldr summer 0 data

        da =
            min (2 * pi) (max (-2 * pi) (settings.endAngle - settings.startAngle))

        p =
            min (abs da / toFloat (List.length data)) settings.padAngle

        pa =
            p
                * (if da < 0 then
                    -1

                   else
                    1
                  )

        sortedIndices =
            List.map Tuple.first << List.sortWith (\( _, a ) ( _, b ) -> settings.sortingFn a b) << List.indexedMap Tuple.pair

        dataArray =
            Array.fromList data

        k =
            if sum == 0 then
                0

            else
                (da - toFloat (List.length data) * pa) / sum

        computeValue el angle =
            let
                value =
                    settings.valueFn el
            in
            { innerRadius = settings.innerRadius
            , outerRadius = settings.outerRadius
            , cornerRadius = settings.cornerRadius
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
                    unsafeGet index array

        helper index ( angle, result ) =
            let
                r =
                    computeValue (unsafeGet index dataArray) angle
            in
            ( r.endAngle, Dict.insert index r result )
    in
    sortedIndices data
        |> List.foldl helper ( settings.startAngle, Dict.empty )
        |> Tuple.second
        |> Dict.values
