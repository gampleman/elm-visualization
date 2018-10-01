module Scale.Sequential exposing (convert)


convert : ( Float, Float ) -> (Float -> a) -> Float -> a
convert ( x0, x1 ) interpolator x =
    interpolator ((x - x0) / (x1 - x0))
