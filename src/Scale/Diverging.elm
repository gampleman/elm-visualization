module Scale.Diverging exposing (log, power, scale, symlog)

import Scale.Continuous


convertWithTransform : (Float -> Float) -> ( Float, Float, Float ) -> (Float -> a) -> Float -> a
convertWithTransform transform ( x0, x1, x2 ) interpolator x =
    let
        t0 =
            transform x0

        t1 =
            transform x1

        t2 =
            transform x2

        xt =
            transform x

        s =
            if t1 < t0 then
                -1

            else
                1

        factor =
            if s * xt < s * t1 then
                if t0 == t1 then
                    0

                else
                    0.5 / (t1 - t0)

            else if t1 == t2 then
                0

            else
                0.5 / (t2 - t1)
    in
    interpolator (0.5 + (xt - t1) * factor)



-- this is just `scaleWithTransform identity`


scaleWithTransform transform interpolator domain =
    { domain = domain
    , range = interpolator
    , convert = convertWithTransform transform
    }


scale =
    scaleWithTransform identity


log _ interpolator (( x0, _, _ ) as domain) =
    let
        transform x =
            if x0 < 0 then
                -(logBase e -x)

            else
                logBase e x
    in
    scaleWithTransform transform interpolator domain


symlog =
    Scale.Continuous.transformSymlog >> scaleWithTransform


power =
    Scale.Continuous.transformPow >> scaleWithTransform
