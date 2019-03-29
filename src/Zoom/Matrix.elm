module Zoom.Matrix exposing (Matrix2x3, transform)


type alias Matrix2x3 =
    ( ( Float, Float, Float ), ( Float, Float, Float ) )


transform : ( Float, Float ) -> Matrix2x3 -> ( Float, Float )
transform ( x, y ) ( ( a11, a12, a13 ), ( a21, a22, a23 ) ) =
    ( a11 * x + a12 * y + a13 * 1, a21 * x + a22 * y + a23 * 1 )
