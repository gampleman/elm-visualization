module Visualization.Path exposing (PathSegment(..), Path, begin, moveTo, lineTo, close, quadraticCurveTo, bezierCurveTo, arcTo, arc, rect, toAttrString, toOneTruePath)

{-| This module provides an abstraction over drawing complex paths. Currently it
contains a function to convert this representation into a string suitable for the
`d` attribute of the `path` SVG element. However, the ADT that powers this is
publicly exposed and alternative renderers can be built in e.g. Canvas or WebGL.

The functions here are modeled after the [Canvas API](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D#Paths).

*Deprecated:* In a future major release this module will be removed in favor of [folkertdev/one-true-path-experiment][otp] or
it's successors, as they provide a safe, high quality wrapper over path based drawing that should be interoperable with
other packages. To make migration easier, the `toOneTruePath` function is provided, so that downstream code can
start working with [folkertdev/one-true-path-experiment][otp],
rather than the datatypes provided in this module, yet still inter-operate.


# Datatype

@docs PathSegment, Path


# Converting

@docs toAttrString, toOneTruePath


# DSL

The DSL can be used interchangebly with directly writing the datatype above.

    begin
      |> moveTo 30 50
      |> lineTo 20 70
      |> lineTo 40 23
      |> close

Is equivalent to:

    --> [Move (30, 50), Line (20, 70), Line (40, 23), Close]

@docs begin, moveTo, lineTo, close, quadraticCurveTo, bezierCurveTo, arcTo, arc, rect

-}

import LowLevel.Command as Cmd
import Path
import SubPath exposing (SubPath)


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


toOneTruePathSegment : PathSegment -> ( List ( Cmd.MoveTo, List Cmd.DrawTo ), Float, Float, Float, Float, Bool ) -> ( List ( Cmd.MoveTo, List Cmd.DrawTo ), Float, Float, Float, Float, Bool )
toOneTruePathSegment item ( subpaths, x0, y0, x1, y1, empty ) =
    let
        append i ds =
            case ds of
                ( moveto, list ) :: more ->
                    ( moveto, list ++ [ i ] ) :: more

                _ ->
                    ds

        convertArc : Float -> Float -> Float -> Float -> Float -> ( List ( Cmd.MoveTo, List Cmd.DrawTo ), Float, Float, Float, Float, Bool )
        convertArc x1_ y1_ x2_ y2_ radius =
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

                subpaths_ =
                    if abs (t01 - 1) > epsilon then
                        append (Cmd.LineTo [ ( x1_ + t01 * x01, y1_ + t01 * y01 ) ]) subpaths
                    else
                        subpaths
            in
                if empty then
                    ( ( Cmd.MoveTo ( x1_, y1_ ), [] ) :: subpaths, x0, y0, x1_, y1_, False )
                else if l01_2 < epsilon then
                    ( subpaths, x0, y0, x1, y1, empty )
                    -- do nothing
                else if not (abs (y01 * x21 - y21 * x01) > epsilon) || r == 0 then
                    ( append (Cmd.LineTo [ ( x1_, y1_ ) ]) subpaths, x0, y0, x1_, y1_, False )
                else
                    ( append
                        (Cmd.arcTo
                            [ { radii = ( r, r )
                              , xAxisRotate = 0
                              , arcFlag = Cmd.smallestArc
                              , direction = boolToDirection (y01 * x20 > x01 * y20)
                              , target = ( x1_ + t21 * x21, y1_ + t21 * y21 )
                              }
                            ]
                        )
                        subpaths_
                    , x0
                    , y0
                    , x1_ + t21 * x21
                    , y1_ + t21 * y21
                    , False
                    )

        boolToDirection b =
            if b then
                Cmd.counterClockwise
            else
                Cmd.clockwise

        boolToArc b =
            if b then
                Cmd.largestArc
            else
                Cmd.smallestArc

        convertArcCustom x y radius a0 a1 ccw =
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

                subpaths_ =
                    if empty then
                        ( Cmd.MoveTo ( x0_, y0_ ), [] ) :: subpaths
                    else if abs (x1 - x0_) > epsilon || abs (y1 - y0_) > epsilon then
                        append (Cmd.LineTo [ ( x0_, y0_ ) ]) subpaths
                    else
                        subpaths
            in
                if r == 0 then
                    -- Is this arc empty? We’re done.
                    ( subpaths_, x0, y0, x1, y1, empty )
                else if da > (tau - epsilon) then
                    -- Is this a complete circle? Draw two arcs to complete the circle.
                    ( append
                        (Cmd.arcTo
                            [ { radii = ( r, r )
                              , xAxisRotate = 0
                              , arcFlag = Cmd.largestArc
                              , direction = cw
                              , target = ( x - dx, y - dy )
                              }
                            ]
                        )
                        subpaths_
                        |> append
                            (Cmd.arcTo
                                [ { radii = ( r, r )
                                  , xAxisRotate = 0
                                  , arcFlag = Cmd.largestArc
                                  , direction = cw
                                  , target = ( x0_, y0_ )
                                  }
                                ]
                            )
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
                        ( append
                            (Cmd.arcTo
                                [ { radii = ( r, r )
                                  , xAxisRotate = 0
                                  , arcFlag = boolToArc (da_ >= pi)
                                  , direction = cw
                                  , target = ( x + r * cos a1, y + r * sin a1 )
                                  }
                                ]
                            )
                            subpaths_
                        , x0
                        , y0
                        , x + r * cos a1
                        , y + r * sin a1
                        , False
                        )
    in
        case item of
            Move ( x, y ) ->
                ( ( Cmd.MoveTo ( x, y ), [] ) :: subpaths, x, y, x, y, False )

            Close ->
                if empty then
                    ( subpaths, x0, y0, x1, y1, empty )
                    -- do nothing
                else
                    ( append Cmd.ClosePath subpaths, x0, y0, x0, y0, False )

            Line ( x, y ) ->
                ( append (Cmd.LineTo [ ( x, y ) ]) subpaths, x0, y0, x, y, False )

            QuadraticCurve cp ( x, y ) ->
                ( append (Cmd.quadraticCurveTo [ ( cp, ( x, y ) ) ]) subpaths, x0, y0, x, y, False )

            BezierCurve cp1 cp2 ( x, y ) ->
                ( append (Cmd.cubicCurveTo [ ( cp1, cp2, ( x, y ) ) ]) subpaths, x0, y0, x, y, False )

            Arc ( x1_, y1_ ) ( x2_, y2_ ) radius ->
                convertArc x1_ y1_ x2_ y2_ radius

            ArcCustom ( x, y ) radius startAngle endAngle anticlockwise ->
                convertArcCustom x y radius startAngle endAngle anticlockwise

            Rect ( x, y ) ( w, h ) ->
                ( ( Cmd.MoveTo ( x, y )
                  , [ Cmd.horizontalTo [ x + w ]
                    , Cmd.verticalTo [ y + h ]
                    , Cmd.horizontalTo [ x ]
                    , Cmd.closePath
                    ]
                  )
                    :: subpaths
                , x
                , y
                , x
                , y
                , False
                )


epsilon : Float
epsilon =
    1.0e-6


{-| Transforms a path to a string that can be passed into the `d` attribute of the
`path` SVG element.

    begin
        |> moveTo 100 100
        |> arcTo 200 100 200 200 50
        |> arc 150 150 50 0 pi False
        |> toAttrString
        --> "M100,100 L150,100 A50,50 0 0 1 200,150 A50,50 0 1 1 100,150"

-}
toAttrString : Path -> String
toAttrString path =
    path
        |> toOneTruePath
        |> Path.toString


{-| Transforms the path to a path from the [folkertdev/one-true-path-experiment][otp]
library. This allows you to do fancy math on the path data - with the hope that this will enable animation library authors
to build general purpose transitions between these.

[otp]: https://github.com/folkertdev/one-true-path-experiment

-}
toOneTruePath : Path -> Path.Path
toOneTruePath path =
    let
        ( result, _, _, _, _, _ ) =
            List.foldl toOneTruePathSegment ( [], 0, 0, 0, 0, True ) path
    in
        List.reverse result
            |> List.map (uncurry SubPath.subpath)
