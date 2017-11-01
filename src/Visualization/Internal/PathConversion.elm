module Visualization.Internal.PathConversion exposing (..)

import List.Extra
import LowLevel.Command exposing (CursorState, DrawTo(..), MoveTo(..), counterClockwise)
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
        LineSegment start end ->
            P.Line end

        Quadratic start c1 end ->
            P.QuadraticCurve c1 end

        Cubic start c1 c2 end ->
            P.BezierCurve c1 c2 end

        Arc { start, end, radii, xAxisRotate, arcFlag, direction } ->
            let
                center =
                    Vector2.sub end start
                        |> Vector2.divideBy 2
                        |> Vector2.add start

                r =
                    Tuple.first radii

                anticlockwise =
                    direction == counterClockwise

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
