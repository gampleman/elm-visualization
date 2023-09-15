module Force.Jiggle exposing (jiggle, jiggleVector)

{-| prevents a value being exactly zero
-}

import Vector2d exposing (Vector2d)


jiggle : Float -> Float
jiggle v =
    if v == 0 then
        1.0e-6

    else
        v


jiggleVector : Vector2d units coordinates -> Vector2d units coordinates
jiggleVector vec =
    let
        { x, y } =
            Vector2d.unwrap vec
    in
    Vector2d.unsafe { x = jiggle x, y = jiggle y }
