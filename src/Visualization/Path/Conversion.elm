module Visualization.Path.Conversion exposing (subpathToVizPath)

import List.Extra
import LowLevel.Command exposing (CursorState, DrawTo(..), MoveTo(..), counterClockwise)
import OpenSolid.CubicSpline2d as CubicSpline2d
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.Point2d exposing (coordinates)
import OpenSolid.QuadraticSpline2d as QuadraticSpline2d
import Segment exposing (Segment(..))
import SubPath exposing (SubPath)
import Vector2
import Visualization.Path as P exposing (Path, PathSegment)


subpathToVizPath : SubPath -> Path
subpathToVizPath subPath =
    SubPath.toSegments subPath
        |> List.map convertSegment
        |> finalize subPath


convertSegment : Segment -> PathSegment
convertSegment segment =
    case segment of
        LineSegment lineSegment ->
            lineSegment
                |> LineSegment2d.endPoint
                |> coordinates
                |> P.Line

        Quadratic quadraticSpline2d ->
            let
                ( start, c1, end ) =
                    QuadraticSpline2d.controlPoints quadraticSpline2d
            in
                P.QuadraticCurve (coordinates c1) (coordinates end)

        Cubic cubicSpline2d ->
            let
                ( start, c1, c2, end ) =
                    CubicSpline2d.controlPoints cubicSpline2d
            in
                P.BezierCurve (coordinates c1) (coordinates c2) (coordinates end)

        Arc { start, end, radii, xAxisRotate, arcFlag, direction } ->
            let
                center =
                    Vector2.sub end start
                        |> Vector2.divideBy 2
                        |> Vector2.add start

                r =
                    Tuple.first radii

                anticlockwise =
                    direction /= counterClockwise

                startAngle =
                    Vector2.angle start center

                endAngle =
                    Vector2.angle end center
            in
                if Tuple.first radii == Tuple.second radii then
                    P.ArcCustom center r startAngle endAngle anticlockwise
                else
                    Debug.log "Unequal radii are not supported" <| P.ArcCustom center r startAngle endAngle anticlockwise


finalize : SubPath -> Path -> Path
finalize subPath path =
    case SubPath.unwrap subPath of
        Just { moveto, drawtos } ->
            let
                (MoveTo start) =
                    moveto
            in
                case List.Extra.last drawtos of
                    Just ClosePath ->
                        P.Move start :: P.close path

                    _ ->
                        P.Move start :: path

        Nothing ->
            []
