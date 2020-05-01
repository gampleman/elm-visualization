module Scale.Symlog exposing (scale)

import Scale.Internal as Continuous
import Scale.Linear as Linear


transformSymlog c x =
    if x < 0 then
        -(logBase e (abs (x / c) + 1))

    else
        logBase e (abs (x / c) + 1)


transformSymexp c x =
    if x < 0 then
        -(e ^ -x - 1) * c

    else
        (e ^ x - 1) * c


scale c range_ domain_ =
    { domain = domain_
    , range = range_
    , convert = Continuous.convertTransform (transformSymlog c) Continuous.interpolateFloat
    , invert = Continuous.invertTransform (transformSymlog c) (transformSymexp c)
    , ticks = Linear.ticks
    , tickFormat = Linear.tickFormat
    , nice = Linear.nice
    , rangeExtent = \_ r -> r
    }
