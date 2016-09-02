module Visualization.Shape exposing (line, linearCurve, Curve)

{-| Visualizations typically consist of discrete graphical marks, such as symbols,
arcs, lines and areas. While the rectangles of a bar chart may be easy enough to
generate directly using SVG or Canvas, other shapes are complex, such as rounded
annular sectors and centripetal Catmull–Rom splines. This module provides a
variety of shape generators for your convenience.

**Note:** Currently only rudimentary forms are implemented.

# Lines

@docs line

# Curves

@docs linearCurve, Curve

-}

import Visualization.Path as Path exposing (..)


type alias Point =
    ( Float, Float )


{-| A curve is represented as a list of points, which a curve function can turn
into drawing commands.
-}
type Curve
    = Line (List Point)



-- | Area (List Point) (List Point)


{-| Produces a polyline through the specified points.
-}
linearCurve : Curve -> List PathSegment
linearCurve part =
    case part of
        Line [] ->
            []

        Line (point :: points) ->
            Path.Move point :: List.map Path.Line points


{-| Generates a line for the given array of points which can be passed to the `d`
attribute of the `path` SVG element. It needs to be suplied with a curve function.
Points accepted are `Maybe`s, Nothing represent gaps in the data and corresponding
gaps will be rendered in the line.

**Note:** A single point (surrounded by Nothing) may not be visible.
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
