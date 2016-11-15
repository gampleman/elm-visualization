module Visualization.Path exposing (PathSegment(..), Path, begin, moveTo, lineTo, close, quadraticCurveTo, bezierCurveTo, arcTo, arc, rect, toAttrString)

{-| This module provides an abstraction over drawing complex paths. Currently it
contains a function to convert this representation into a string suitable for the
`d` attribute of the `path` SVG element. However, the ADT that powers this is
publicly exposed and alternative renderers can be built in e.g. Canvas or WebGL.

The functions here are modeled after the [Canvas API](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D#Paths).

# Datatype

@docs PathSegment, Path

# Converting

@docs toAttrString

# DSL

The DSL can be used interchangebly with directly writing the datatype above.

    begin
      |> moveTo 30 50
      |> lineTo 20 70
      |> lineTo 40 23
      |> close

Is equivalent to:

    [Move (30, 50), Line (20, 70), Line (40, 23), Close]

@docs begin, moveTo, lineTo, close, quadraticCurveTo, bezierCurveTo, arcTo, arc, rect

-}

import String


type alias Point =
    ( Float, Float )


{-| A list of `PathSegment`s represents a path. These are essentially drawing
commands that are based on the Canvas API. For more information, see the DSL
section in this module.
-}
type PathSegment
    = Move Point
    | Close
    | Line Point
    | QuadraticCurve Point Point
    | BezierCurve Point Point Point
    | Arc Point Point Float
    | ArcCustom Point Float Float Float Bool
    | Rect Point ( Float, Float )


{-| -}
type alias Path =
    List PathSegment


push : PathSegment -> Path -> Path
push el list =
    list ++ [ el ]


{-| Start a new path. Equivalent to `[]`.
-}
begin : Path
begin =
    []


{-| Move to the specified point ⟨x, y⟩ and start a new subpath.

**Note:** In SVG having multiple `moveTo` commands following each other draws
an implicit path between them. Do not rely on this behavior, in the future this
may be removed.
-}
moveTo : Float -> Float -> Path -> Path
moveTo x y =
    push <| Move ( x, y )


{-| Ends the current subpath and causes an automatic straight line to be drawn
from the current point to the initial point of the current subpath.
-}
close : Path -> Path
close =
    push Close


{-| Draws a straight line from the current point to the specified point ⟨x, y⟩.
-}
lineTo : Float -> Float -> Path -> Path
lineTo x y =
    push <| Line ( x, y )


{-| Draws a quadratic Bézier segment from the current point to the specified
point ⟨x, y⟩, with the specified control point ⟨cpx, cpy⟩.

    quadraticCurveTo cpx cpy x y
-}
quadraticCurveTo : Float -> Float -> Float -> Float -> Path -> Path
quadraticCurveTo cpx cpy x y =
    push <| QuadraticCurve ( cpx, cpy ) ( x, y )


{-| Draws a cubic Bézier segment from the current point to the specified
point ⟨x, y⟩, with the specified control points ⟨cpx1, cpy1⟩ and ⟨cpx2, cpy2⟩.

    bezierCurveTo cpx1 cpy1 cpx2 cpy2 x y
-}
bezierCurveTo : Float -> Float -> Float -> Float -> Float -> Float -> Path -> Path
bezierCurveTo cpx1 cpy1 cpx2 cpy2 x y =
    push <| BezierCurve ( cpx1, cpy1 ) ( cpx2, cpy2 ) ( x, y )


{-| Draws a circular arc segment with the specified radius that starts tangent
to the line between the current point and the specified point ⟨x1, y1⟩ and ends
tangent to the line between the specified points ⟨x1, y1⟩ and ⟨x2, y2⟩. If the
first tangent point is not equal to the current point, a straight line is drawn
between the current point and the first tangent point.

     arcTo x1 y1 x2 y2 radius
-}
arcTo : Float -> Float -> Float -> Float -> Float -> Path -> Path
arcTo x1 y1 x2 y2 radius =
    push <| Arc ( x1, y1 ) ( x2, y2 ) radius


{-| Draws a circular arc segment with the specified center ⟨x, y⟩, radius,
startAngle and endAngle. If anticlockwise is true, the arc is drawn in the
anticlockwise direction; otherwise, it is drawn in the clockwise direction.
If the current point is not equal to the starting point of the arc, a straight
line is drawn from the current point to the start of the arc.

    arc x y radius startAngle endAngle anticlockwise
-}
arc : Float -> Float -> Float -> Float -> Float -> Bool -> Path -> Path
arc x y radius startAngle endAngle anticlockwise =
    push <| ArcCustom ( x, y ) radius startAngle endAngle anticlockwise


{-| Creates a new subpath containing just the four points ⟨x, y⟩, ⟨x + w, y⟩,
⟨x + w, y + h⟩, ⟨x, y + h⟩, with those four points connected by straight lines,
and then marks the subpath as closed.

    rect x y w h
-}
rect : Float -> Float -> Float -> Float -> Path -> Path
rect x y w h =
    push <| Rect ( x, y ) ( w, h )



{- Modulus for Floats. -}


mod : Float -> Float -> Float
mod a b =
    let
        frac =
            a / b
    in
        (frac - toFloat (truncate frac)) * b


stringify : PathSegment -> ( String, Float, Float, Float, Float, Bool ) -> ( String, Float, Float, Float, Float, Bool )
stringify item ( str, x0, y0, x1, y1, empty ) =
    let
        append cmd values str =
            str ++ cmd ++ (String.join "," <| List.map toString values)

        epsilon =
            1.0e-6

        stringifyArc x1_ y1_ x2_ y2_ radius =
            let
                -- TODO: Figure out how this actually works and write a lot of comments/refactor.
                -- Currently this is a straight port from D3.
                r =
                    abs radius

                x0_ =
                    x1

                y0_ =
                    y1

                x21 =
                    x2_ - x1_

                y21 =
                    y2_ - y1_

                x01 =
                    x0_ - x1_

                y01 =
                    y0_ - y1_

                l01_2 =
                    x01 ^ 2 + y01 ^ 2

                x20 =
                    x2_ - x0_

                y20 =
                    y2_ - y0_

                l21_2 =
                    x21 ^ 2 + y21 ^ 2

                l20_2 =
                    x20 ^ 2 + y20 ^ 2

                l21 =
                    sqrt l21_2

                l01 =
                    sqrt l01_2

                l =
                    r * tan ((pi - acos ((l21_2 + l01_2 - l20_2) / (2 * l21 * l01))) / 2)

                t01 =
                    l / l01

                t21 =
                    l / l21

                str_ =
                    if abs (t01 - 1) > epsilon then
                        append "L" [ x1_ + t01 * x01, y1_ + t01 * y01 ] str
                    else
                        str
            in
                if empty then
                    ( append "M" [ x1_, y1_ ] str, x0, y0, x1_, y1_, False )
                else if l01_2 < epsilon then
                    ( str, x0, y0, x1, y1, empty )
                    -- do nothing
                else if not (abs (y01 * x21 - y21 * x01) > epsilon) || r == 0 then
                    ( append "L" [ x1_, y1_ ] str, x0, y0, x1_, y1_, False )
                else
                    ( append "A"
                        [ r
                        , r
                        , 0
                        , 0
                        , (if y01 * x20 > x01 * y20 then
                            1
                           else
                            0
                          )
                        , x1_ + t21 * x21
                        , y1_ + t21 * y21
                        ]
                        str_
                    , x0
                    , y0
                    , x1_ + t21 * x21
                    , y1_ + t21 * y21
                    , False
                    )

        boolToFloat b =
            if b then
                1
            else
                0

        stringifyArcCustom x y radius a0 a1 ccw =
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
                    boolToFloat (not ccw)

                tau =
                    2 * pi

                da =
                    if ccw then
                        a0 - a1
                    else
                        a1 - a0

                str_ =
                    if empty then
                        append "M" [ x0_, y0_ ] str
                    else if abs (x1 - x0_) > epsilon || abs (y1 - y0_) > epsilon then
                        append "L" [ x0_, y0_ ] str
                    else
                        str
            in
                if r == 0 then
                    -- Is this arc empty? We’re done.
                    ( str_, x0, y0, x1, y1, empty )
                else if da > (tau - epsilon) then
                    -- Is this a complete circle? Draw two arcs to complete the circle.
                    ( append "A" [ r, r, 0, 1, cw, x - dx, y - dy ] str_
                        |> append "A" [ r, r, 0, 1, cw, x0_, y0_ ]
                    , x0
                    , y0
                    , x0_
                    , y0_
                    , False
                    )
                else
                    let
                        da_ =
                            if da < 0 then
                                (mod da tau) + tau
                            else
                                da
                    in
                        -- Otherwise, draw an arc!
                        ( append "A" [ r, r, 0, boolToFloat (da_ >= pi), cw, x + r * cos a1, y + r * sin a1 ] str_, x0, y0, x + r * cos a1, y + r * sin a1, False )
    in
        case item of
            Move ( x, y ) ->
                ( append "M" [ x, y ] str, x, y, x, y, False )

            Close ->
                if empty then
                    ( str, x0, y0, x1, y1, empty )
                    -- do nothing
                else
                    ( append "Z" [] str, x0, y0, x0, y0, False )

            Line ( x, y ) ->
                ( append "L" [ x, y ] str, x0, y0, x, y, False )

            QuadraticCurve ( cpx, cpy ) ( x, y ) ->
                ( append "Q" [ cpx, cpy, x, y ] str, x0, y0, x, y, False )

            BezierCurve ( cpx1, cpy1 ) ( cpx2, cpy2 ) ( x, y ) ->
                ( append "C" [ cpx1, cpy1, cpx2, cpy2, x, y ] str, x0, y0, x, y, False )

            Arc ( x1_, y1_ ) ( x2_, y2_ ) radius ->
                stringifyArc x1_ y1_ x2_ y2_ radius

            ArcCustom ( x, y ) radius startAngle endAngle anticlockwise ->
                stringifyArcCustom x y radius startAngle endAngle anticlockwise

            Rect ( x, y ) ( w, h ) ->
                ( append "M" [ x, y ] str
                    |> append "h" [ w ]
                    |> append "v" [ h ]
                    |> append "h" [ -w ]
                    |> append "Z" []
                , x
                , y
                , x
                , y
                , False
                )


{-| Transforms a path to a string that can be passed into the `d` attribute of the
`path` SVG element.

    path
        |> moveTo 100 100
        |> arcTo 200 100 200 200 50
        |> arc 150 150 50 0 pi False
        |> toAttrString
        -- "M100,100L150,100A50,50,0,0,1,200,150A50,50,0,1,1,100,150"
-}
toAttrString : Path -> String
toAttrString path =
    let
        ( result, _, _, _, _, _ ) =
            List.foldl stringify ( "", 0, 0, 0, 0, True ) path
    in
        result
