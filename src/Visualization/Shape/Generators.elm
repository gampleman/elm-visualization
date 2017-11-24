module Visualization.Shape.Generators exposing (..)

import Visualization.Path exposing (toAttrString, lineTo, close)


line :
    (List a -> List Visualization.Path.PathSegment)
    -> List (Maybe a)
    -> String
line curve data =
    let
        makeCurves datum ( prev, list ) =
            case ( prev, datum, list ) of
                ( _, Nothing, l ) ->
                    ( False, l )

                ( False, Just point, l ) ->
                    ( True, [ point ] :: l )

                ( True, Just p1, ps :: l ) ->
                    ( True, (p1 :: ps) :: l )

                ( True, Just p1, l ) ->
                    ( True, [ p1 ] :: l )
    in
        toAttrString <| List.concatMap curve <| Tuple.second <| List.foldr makeCurves ( False, [] ) data


area :
    (List ( Float, Float ) -> List Visualization.Path.PathSegment)
    -> List (Maybe ( ( Float, Float ), ( Float, Float ) ))
    -> String
area curve data =
    let
        makeCurves acc datum ( prev, list ) =
            case ( prev, datum, list ) of
                ( _, Nothing, l ) ->
                    ( False, l )

                ( False, Just point, l ) ->
                    ( True, [ acc point ] :: l )

                ( True, Just p1, ps :: l ) ->
                    ( True, (acc p1 :: ps) :: l )

                ( True, Just p1, l ) ->
                    ( True, [ acc p1 ] :: l )

        topLineData =
            List.foldr (makeCurves Tuple.first) ( False, [] ) data
                |> Tuple.second

        bottomLineData =
            List.foldr (makeCurves Tuple.second) ( False, [] ) data
                |> Tuple.second
                |> List.map List.reverse

        makeShape topline bottomline =
            case ( curve bottomline, bottomline ) of
                ( _ :: tail, ( x, y ) :: _ ) ->
                    (curve topline |> lineTo x y) ++ (tail |> close)

                _ ->
                    []

        shapes =
            List.map2 makeShape topLineData bottomLineData
    in
        List.map2 makeShape topLineData bottomLineData
            |> List.concat
            |> toAttrString
