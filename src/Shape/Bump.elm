module Shape.Bump exposing (bumpXCurve, bumpYCurve)

import LowLevel.Command exposing (cubicCurveTo, moveTo)
import SubPath exposing (SubPath)


bumpXCurve : List ( Float, Float ) -> SubPath
bumpXCurve points =
    case points of
        [] ->
            SubPath.empty

        first :: rest ->
            SubPath.with (moveTo first)
                [ List.foldl
                    (\( x, y ) ( ( x0, y0 ), soFar ) ->
                        let
                            x1 =
                                (x0 + x) / 2
                        in
                        ( ( x, y ), ( ( x1, y0 ), ( x1, y ), ( x, y ) ) :: soFar )
                    )
                    ( first, [] )
                    rest
                    |> Tuple.second
                    |> List.reverse
                    |> cubicCurveTo
                ]


bumpYCurve : List ( Float, Float ) -> SubPath
bumpYCurve points =
    case points of
        [] ->
            SubPath.empty

        first :: rest ->
            SubPath.with (moveTo first)
                [ List.foldl
                    (\( x, y ) ( ( x0, y0 ), soFar ) ->
                        let
                            y1 =
                                (y0 + y) / 2
                        in
                        ( ( x, y ), ( ( x0, y1 ), ( x, y1 ), ( x, y ) ) :: soFar )
                    )
                    ( first, [] )
                    rest
                    |> Tuple.second
                    |> List.reverse
                    |> cubicCurveTo
                ]



{-
   Do we need this? Can be published later...


   pointRadial : Float -> Float -> ( Float, Float )
   pointRadial x y =
       let
           x1 =
               x - pi / 2
       in
       ( y * cos x1, y * sin x1 )


   bumpRadialCurve : List ( Float, Float ) -> SubPath
   bumpRadialCurve points =
       case points of
           [] ->
               SubPath.empty

           ( fx, fy ) :: rest ->
               SubPath.with (moveTo (pointRadial fx fy))
                   [ List.foldl
                       (\( x, y ) ( ( x0, y0 ), soFar ) ->
                           let
                               y1 =
                                   (y0 + y) / 2
                           in
                           ( ( x, y ), ( pointRadial x0 y1, pointRadial x y1, pointRadial x y ) :: soFar )
                       )
                       ( ( fx, fy ), [] )
                       rest
                       |> Tuple.second
                       |> List.reverse
                       |> cubicCurveTo
                   ]
-}
