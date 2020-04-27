module Scale.Power exposing (scale)

import Scale.Internal as Continuous
import Scale.Linear as Linear


transformPow exponent x =
    if x < 0 then
        -(-x ^ exponent)

    else
        x ^ exponent


scale exponent range_ domain_ =
    { domain = domain_
    , range = range_
    , convert = Continuous.convertTransform (transformPow exponent) Continuous.interpolateFloat
    , invert = Continuous.invertTransform (transformPow exponent) (transformPow (1 / exponent))
    , ticks = Linear.ticks
    , tickFormat = Linear.tickFormat
    , nice = Linear.nice
    , rangeExtent = \_ r -> r
    }
