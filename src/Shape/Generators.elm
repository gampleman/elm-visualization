module Shape.Generators exposing (area, line)

import Path exposing (Path)
import SubPath exposing (SubPath)


line :
    (List a -> SubPath)
    -> List (Maybe a)
    -> Path
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
    List.map curve <| Tuple.second <| List.foldr makeCurves ( False, [] ) data


area :
    (List ( Float, Float ) -> SubPath)
    -> List (Maybe ( ( Float, Float ), ( Float, Float ) ))
    -> Path
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
            curve topline |> SubPath.connect (curve bottomline)

        shapes =
            List.map2 makeShape topLineData bottomLineData
    in
    List.map2 makeShape topLineData bottomLineData
