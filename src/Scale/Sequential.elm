module Scale.Sequential exposing (log, power, scale, symlog)

import Scale.Continuous


convert : ( Float, Float ) -> (Float -> a) -> Float -> a
convert ( x0, x1 ) interpolator x =
    interpolator ((x - x0) / (x1 - x0))


convertWithTransform : (Float -> Float) -> ( Float, Float ) -> (Float -> a) -> Float -> a
convertWithTransform transform ( x0, x1 ) interpolator x =
    let
        t0 =
            transform x0

        t1 =
            transform x1
    in
    interpolator ((transform x - t0) / (t1 - t0))



-- this is just `scaleWithTransform identity`


scale interpolator domain =
    { domain = domain
    , range = interpolator
    , convert = convert
    }


scaleWithTransform transform interpolator domain =
    { domain = domain
    , range = interpolator
    , convert = convertWithTransform transform
    }


log _ interpolator domain =
    let
        transform x =
            if Tuple.first domain < 0 then
                -(logBase e -x)

            else
                logBase e x
    in
    scaleWithTransform transform interpolator domain


symlog =
    Scale.Continuous.transformSymlog >> scaleWithTransform


power =
    Scale.Continuous.transformPow >> scaleWithTransform
