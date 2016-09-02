module Visualization.Shape exposing (..)

import Visualization.Path as Path exposing (..)


type alias Point =
    ( Float, Float )


type CurvePart
    = Line (List Point)



-- | Area (List Point) (List Point)


linearCurve : CurvePart -> List PathSegment
linearCurve part =
    case part of
        Line [] ->
            []

        Line (point :: points) ->
            List.reverse (Path.Move point :: List.map Path.Line points)


line : (CurvePart -> List PathSegment) -> List (Maybe Point) -> String
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
