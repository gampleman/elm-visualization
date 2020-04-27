module Scale.Radial exposing (scale)

import Scale.Linear as Linear


scale range_ domain_ =
    { domain = domain_
    , range = range_
    , convert = \d r -> Linear.convert d (squareRange r) >> unsquare
    , invert = \d r -> square >> Linear.invert d (squareRange r)
    , ticks = Linear.ticks
    , tickFormat = Linear.tickFormat
    , nice = Linear.nice
    , rangeExtent = \_ r -> r
    }


withSquaredRange f domain ( r0, r1 ) =
    f domain ( square r0, square r1 )


squareRange ( a, b ) =
    ( square a, square b )


square : Float -> Float
square x =
    (if x >= 0 then
        1

     else
        -1
    )
        * x
        * x


unsquare x =
    (if x >= 0 then
        1

     else
        -1
    )
        * sqrt (abs x)
