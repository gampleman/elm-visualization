module Zoom.Interpolation exposing (interpolate)

{-| This is again one of those crazy ports from D3 modules...
-}


type alias View =
    { cx : Float
    , cy : Float
    , size : Float
    }


exp : Float -> Float
exp x =
    e ^ x


log : Float -> Float
log =
    logBase e


epsilon2 : Float
epsilon2 =
    1.0e-12


rho : Float
rho =
    sqrt 2


cosh : Float -> Float
cosh x =
    let
        x_ =
            exp x
    in
    (x_ + 1 / x_) / 2


sinh : Float -> Float
sinh x =
    let
        x_ =
            exp x
    in
    (x_ - 1 / x_) / 2


tanh : Float -> Float
tanh x =
    let
        x_ =
            exp (2 * x)
    in
    (x_ - 1) / (x_ + 1)


interpolate : View -> View -> ( Float, Float -> View )
interpolate a b =
    let
        dx =
            b.cx - a.cx

        dy =
            b.cy - a.cy

        d2 =
            dx ^ 2 + dy ^ 2
    in
    -- special case for a.cxy ≅ b.cxy
    if d2 < epsilon2 then
        let
            s =
                log (b.size / a.size)
                    / rho
        in
        ( abs s * 1000
        , \t ->
            { cx = a.cx + t * dx
            , cy = a.cy + t * dy
            , size = a.size * exp (rho * t * s)
            }
        )

    else
        -- general case
        let
            d1 =
                sqrt d2

            b0 =
                (b.size ^ 2 - (a.size ^ 2) + rho ^ 4 * d2) / (2 * a.size * rho ^ 2 * d1)

            b1 =
                (b.size ^ 2 - (a.size ^ 2) - rho ^ 4 * d2) / (2 * b.size * rho ^ 2 * d1)

            r0 =
                log (sqrt (b0 ^ 2 + 1) - b0)

            r1 =
                log (sqrt (b1 ^ 2 + 1) - b1)

            s_ =
                (r1 - r0)
                    / rho
        in
        ( s_ * 1000
        , \t ->
            let
                s =
                    t * s_

                coshr0 =
                    cosh r0

                u =
                    a.size / (rho ^ 2 * d1) * (coshr0 * tanh (rho * s + r0) - sinh r0)
            in
            { cx = a.cx + u * dx
            , cy = a.cy + u * dy
            , size = a.size * coshr0 / cosh (rho * s + r0)
            }
        )
