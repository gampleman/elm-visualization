module Force.Jiggle exposing (jiggle)

{-| prevents a value being exactly zero
-}


jiggle : Float -> Float
jiggle v =
    if v == 0 then
        1.0e-6

    else
        v
