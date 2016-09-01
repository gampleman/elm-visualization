module Visualization.Path exposing (..)

import String


type alias Point =
    ( Float, Float )


type PathSegment
    = Move Point
    | Close
    | Line Point
    | QuadraticCurve Point Point
    | BezierCurve Point Point Point
    | Arc Point Point Float
    | ArcCustom Point Float Float Float Bool
    | Rect Point ( Float, Float )


type alias Path =
    List PathSegment


path : Path
path =
    []


moveTo : Float -> Float -> Path -> Path
moveTo x y path =
    Move ( x, y ) :: path


close : Path -> Path
close path =
    Close :: path


lineTo : Float -> Float -> Path -> Path
lineTo x y path =
    Line ( x, y ) :: path


quadraticCurveTo : Float -> Float -> Float -> Float -> Path -> Path
quadraticCurveTo cpx cpy x y path =
    QuadraticCurve ( cpx, cpy ) ( x, y ) :: path


bezierCurveTo : Float -> Float -> Float -> Float -> Float -> Float -> Path -> Path
bezierCurveTo cpx1 cpy1 cpx2 cpy2 x y path =
    BezierCurve ( cpx1, cpy1 ) ( cpx2, cpy2 ) ( x, y ) :: path


arcTo : Float -> Float -> Float -> Float -> Float -> Path -> Path
arcTo x1 y1 x2 y2 radius path =
    Arc ( x1, y1 ) ( x2, y2 ) radius :: path


arc : Float -> Float -> Float -> Float -> Float -> Bool -> Path -> Path
arc x y radius startAngle endAngle anticlockwise path =
    ArcCustom ( x, y ) radius startAngle endAngle anticlockwise :: path


rect : Float -> Float -> Float -> Float -> Path -> Path
rect x y w h path =
    Rect ( x, y ) ( w, h ) :: path


stringify : PathSegment -> ( String, Float, Float, Float, Float, Bool ) -> ( String, Float, Float, Float, Float, Bool )
stringify item ( str, x0, y0, x1, y1, empty ) =
    let
        append cmd values str =
            str ++ cmd ++ (String.join "," <| List.map toString values)

        stringifyArc x1' y1' x2' y2' r =
            let
                epsilon =
                    1.0e-6

                x0' =
                    x1

                y0' =
                    y1

                x21 =
                    x2' - x1'

                y21 =
                    y2' - y1'

                x01 =
                    x0' - x1'

                y01 =
                    y0' - y1'

                l01_2 =
                    x01 * x01 + y01 * y01

                x20 =
                    x2' - x0'

                y20 =
                    y2' - y0'

                l21_2 =
                    x21 * x21 + y21 * y21

                l20_2 =
                    x20 * x20 + y20 * y20

                l21 =
                    sqrt l21_2

                l01 =
                    sqrt l01_2

                l =
                    r * tan (pi - acos ((l21_2 + l01_2 - l20_2) / (2 * l21 * l01)) / 2)

                t01 =
                    l / l01

                t21 =
                    l / l21

                str' =
                    if abs (t01 - 1) > epsilon then
                        append "L" [ x1' + t01 * x01, y1' + t01 * y01 ] str
                    else
                        str
            in
                if l01_2 < epsilon then
                    ( str, x0, y0, x1, y1, empty )
                    -- do nothing
                else if not (abs (y01 * x21 - y21 * x01) > epsilon) || r == 0 then
                    ( append "L" [ x1', y1' ] str, x0, y0, x1', y1', False )
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
                        , x1' + t21 * x21
                        , y1' + t21 * y21
                        ]
                        str'
                    , x0
                    , y0
                    , x1' + t21 * x21
                    , y1' + t21 * y21
                    , False
                    )

        stringifyArcCustom _ =
            Debug.crash "not implemented"
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

            Arc ( x1', y1' ) ( x2', y2' ) radius ->
                stringifyArc x1' y1' x2' y2' radius

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


toAttrString : Path -> String
toAttrString path =
    let
        ( result, _, _, _, _, _ ) =
            List.foldr stringify ( "", 0, 0, 0, 0, True ) path
    in
        result
