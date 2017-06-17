module Visualization.Scale
    exposing
        ( Scale
        , ContinuousScale
        , linear
          -- , power
        , log
        , identity
        , ContinuousTimeScale
        , time
        , SequentialScale
        , sequential
        , QuantizeScale
        , quantize
        , convert
        , invert
        , domain
        , range
        , rangeExtent
        , ticks
        , tickFormat
        , clamp
        , nice
        , invertExtent
        , viridisInterpolator
        )

{-| Scales are a convenient abstraction for a fundamental task in visualization:
mapping a dimension of abstract data to a visual representation. Although most
often used for position-encoding quantitative data, such as mapping a measurement
in meters to a position in pixels for dots in a scatterplot, scales can represent
virtually any visual encoding, such as diverging colors, stroke widths, or symbol
size. Scales can also be used with virtually any type of data, such as named
categorical data or discrete data that requires sensible breaks.

For continuous quantitative data, you typically want a linear scale. (For time
series data, a time scale.) If the distribution calls for it, consider
transforming data using a power or log scale. A quantize scale may aid
differentiation by rounding continuous data to a fixed set of discrete values;
similarly, a quantile scale computes quantiles from a sample population, and a
threshold scale allows you to specify arbitrary breaks in continuous data.
Several built-in sequential color schemes are also provided.

For discrete ordinal (ordered) or categorical (unordered) data, an ordinal scale
specifies an explicit mapping from a set of data values to a corresponding set
of visual attributes (such as colors). The related band and point scales are
useful for position-encoding ordinal data, such as bars in a bar chart or dots
in an categorical scatterplot. Several built-in categorical color scales are
also provided.

Scales have no intrinsic visual representation. However, most scales can generate
and format ticks for reference marks to aid in the construction of [axes](Visualization-Axis).


# General notes

@docs Scale


# Continuous Scales

@docs ContinuousScale, linear, log, identity, ContinuousTimeScale, time

Continuous scales support the following operations:

@docs convert, invert, domain, range, rangeExtent, ticks, tickFormat, clamp, nice


# Sequential Scales

Sequential scales are similar to continuous scales in that they map a continuous,
numeric input domain to a continuous output range. However, unlike continuous
scales, the output range of a sequential scale is fixed by its interpolator function.

@docs SequentialScale, sequential, viridisInterpolator

Sequential scales support the following operations:

@docs convert, domain, rangeExtent


# Quantize Scales

Quantize scales are similar to linear scales, except they use a discrete rather
than continuous range. The continuous input domain is divided into uniform
segments based on the number of values in (i.e., the cardinality of) the output
range. Each range value y can be expressed as a quantized linear function of the
domain value `x`: `y = m round(x) + b`.

@docs QuantizeScale, quantize

Quantize scales support the following operations:

@docs convert, invertExtent, domain, range, rangeExtent, ticks, tickFormat, nice

-}

import Color exposing (Color)
import Date exposing (Date)
import Visualization.Scale.ColorInterpolators as ColorInterpolators
import Visualization.Scale.Linear as Linear
import Visualization.Scale.Log as Log
import Visualization.Scale.Quantize as Quantize
import Visualization.Scale.Sequential as Sequential
import Visualization.Scale.Time as Time


{-| This API is highly polymorphic as each scale has different functions exposed.
This is still done in a convenient and type-safe manner, however the cost is
a certain ugliness and complexity of the type signatures. For this reason the
supported functions are listed again for each category. It is best to ignore the
type signatures when learning about the library.
-}
type Scale scaleSpec
    = Scale scaleSpec



-- Continuous Scales


{-| Type alias for Continuous Scales. These map a `(Float, Float)` **domain** to a
`(Float, Float)` **range**.
-}
type alias ContinuousScale =
    Scale
        { domain : ( Float, Float )
        , range : ( Float, Float )
        , convert : ( Float, Float ) -> ( Float, Float ) -> Float -> Float
        , invert :
            ( Float, Float ) -> ( Float, Float ) -> Float -> Float

        -- , rangeRound : ( Float, Float ) -> ( Float, Float )
        , ticks : ( Float, Float ) -> Int -> List Float
        , tickFormat : ( Float, Float ) -> Int -> Float -> String
        , nice : ( Float, Float ) -> Int -> ( Float, Float )
        , rangeExtent : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
        }


{-| Linear scales are a good default choice for continuous quantitative data
because they preserve proportional differences. Each range value y can be
expressed as a function of the domain value x: y = mx + b.

    scale = linear ( 0, 1 ) ( 50, 100 )
    convert scale 0.5 == 75

-}
linear : ( Float, Float ) -> ( Float, Float ) -> ContinuousScale
linear domain range =
    Scale
        { domain = domain
        , range = range
        , convert = Linear.convert
        , invert = Linear.invert
        , ticks = Linear.ticks
        , tickFormat = Linear.tickFormat
        , nice = Linear.nice
        , rangeExtent = Linear.rangeExtent
        }



-- {-| Power scales are similar to linear scales, except an exponential transform
-- is applied to the input domain value before the output range value is computed.
-- Each range value y can be expressed as a function of the domain value x:
-- y = mx^k + b, where k is the exponent value. Power scales also support negative
-- domain values, in which case the input value and the resulting output value are
-- multiplied by -1.
--
-- The arguments are `exponent`, `domain` and `range`.
--
--     scale = power 2 ( 0, 1 ) ( 50, 100 )
--     convert scale 0.5 = 62.5
-- -}
-- power : Float -> ( Float, Float ) -> ( Float, Float ) -> ContinuousScale
-- power exponent =
--     Debug.crash "not implemented"


{-| Log scales are similar to linear scales, except a logarithmic transform is
applied to the input domain value before the output range value is computed.
The mapping to the range value y can be expressed as a function of the domain
value x: y = m log(x) + b.

As log(0) = -∞, a log scale domain must be strictly-positive or strictly-negative;
the domain must not include or cross zero. A log scale with a positive domain has
a well-defined behavior for positive values, and a log scale with a negative
domain has a well-defined behavior for negative values. (For a negative domain,
input and output values are implicitly multiplied by -1.) The behavior of the
scale is undefined if you pass a negative value to a log scale with a positive
domain or vice versa.

The arguments are `base`, `domain` and `range`.

    scale = log 10 ( 10, 1000 ) ( 50, 100 )
    convert scale 100 == 75

-}
log : Float -> ( Float, Float ) -> ( Float, Float ) -> ContinuousScale
log base domain range =
    Scale
        { domain = domain
        , range = range
        , convert = Log.convert
        , invert = Log.invert
        , ticks = Log.ticks base
        , tickFormat = Log.tickFormat
        , nice = Log.nice base
        , rangeExtent = Log.rangeExtent
        }


{-| Identity scales are a special case of linear scales where the domain and
range are identical; the scale and its invert method are thus the identity function.
These scales are occasionally useful when working with pixel coordinates, say in
conjunction with an axis or brush.
-}
identity : ContinuousScale
identity =
    linear ( 0, 1 ) ( 0, 1 )


{-| This is identical to a ContinuousScale, except the domain values are Dates instead of Floats.
-}
type alias ContinuousTimeScale =
    Scale
        { domain : ( Date, Date )
        , range : ( Float, Float )
        , convert : ( Date, Date ) -> ( Float, Float ) -> Date -> Float
        , invert :
            ( Date, Date ) -> ( Float, Float ) -> Float -> Date

        -- , rangeRound : ( Float, Float ) -> ( Float, Float )
        , ticks : ( Date, Date ) -> Int -> List Date
        , tickFormat : ( Date, Date ) -> Int -> Date -> String
        , nice : ( Date, Date ) -> Int -> ( Date, Date )
        , rangeExtent : ( Date, Date ) -> ( Float, Float ) -> ( Float, Float )
        }


{-| Time scales are a variant of linear scales that have a temporal domain: domain
values are dates rather than floats, and invert likewise returns a date.
Time scales implement ticks based on calendar intervals, taking the pain out of
generating axes for temporal domains.
-}
time : ( Date, Date ) -> ( Float, Float ) -> ContinuousTimeScale
time domain range =
    Scale
        { domain = domain
        , range = range
        , convert = Time.convert
        , invert = Time.invert
        , ticks = Time.ticks
        , tickFormat = Time.tickFormat
        , nice = Time.nice
        , rangeExtent = Time.rangeExtent
        }



-- timeUtc : ( Date, Date ) -> ( Float, Float ) -> Scale (Capabilities Continuous) ( Date, Date ) ( Float, Float )
-- timeUtc range =
--     Debug.crash "not implemented"
-- Sequential Scales


{-| Type alias for sequential scales
-}
type alias SequentialScale a =
    Scale
        { domain : ( Float, Float )
        , range : Float -> a
        , convert : ( Float, Float ) -> (Float -> a) -> Float -> a
        }


{-| -}
sequential : ( Float, Float ) -> (Float -> a) -> SequentialScale a
sequential domain interpolator =
    Scale
        { domain = domain
        , range = interpolator
        , convert = Sequential.convert
        }


{-| -}
viridisInterpolator : Float -> Color
viridisInterpolator =
    ColorInterpolators.viridis



-- Quantize Scales


{-| Type alias for quantize scales
-}
type alias QuantizeScale a =
    Scale
        { domain : ( Float, Float )
        , range :
            ( a, List a )

        -- non-empty list
        , convert : ( Float, Float ) -> ( a, List a ) -> Float -> a
        , invertExtent : ( Float, Float ) -> ( a, List a ) -> a -> Maybe ( Float, Float )
        , ticks : ( Float, Float ) -> ( a, List a ) -> Int -> List Float
        , tickFormat : ( Float, Float ) -> ( a, List a ) -> Int -> Float -> String
        , nice : ( Float, Float ) -> Int -> ( Float, Float )
        , rangeExtent : ( Float, Float ) -> ( a, List a ) -> ( a, a )
        }


{-| -}
quantize : ( Float, Float ) -> ( a, List a ) -> QuantizeScale a
quantize domain range =
    Scale
        { domain = domain
        , range = range
        , convert = Quantize.convert
        , invertExtent = Quantize.invertExtent
        , ticks = Quantize.ticks
        , tickFormat = Quantize.tickFormat
        , nice = Quantize.nice
        , rangeExtent = Quantize.rangeExtent
        }



-- Quantile Scales
--
-- type alias QuantileScale a =
--     Scale Quantile (List Float) (List a)
--
--
-- quantile : List Float -> List a -> QuantileScale a
-- quantile domain =
--     Debug.crash "not implemented"
--
--
--
-- -- Threshold Scales
--
--
-- type alias ThresholdScale comparable a =
--     Scale Threshold (List comparable) (List a)
--
--
-- threshold : List comparable -> List a -> ThresholdScale comparable a
-- threshold domain =
--     Debug.crash "not implemented"
--
--
--
-- -- Ordinal Scales
--
--
-- type alias OrdinalScale a b =
--     Scale Ordinal (List a) (List b)
--
--
-- ordinal : List a -> List b -> OrdinalScale a b
-- ordinal domain =
--     Debug.crash "not implemented"
--
--
--
-- -- Band Scales
--
--
-- type alias BandScale a =
--     Scale Ordinal (List a) ( Float, Float )
--
--
-- band : List a -> ( Float, Float ) -> BandScale a
-- band a =
--     Debug.crash "not implemented"
--
--
-- point : List a -> ( Float, Float ) -> BandScale a
-- point a =
--     Debug.crash "not implemented"
-- Methods


{-| Given a value from the domain, returns the corresponding value from the range.
If the given value is outside the domain the mapping may be extrapolated such
that the returned value is outside the range.
-}
convert : Scale { a | convert : domain -> range -> value -> result, domain : domain, range : range } -> value -> result
convert (Scale scale) value =
    scale.convert scale.domain scale.range value


{-| Given a value from the range, returns the corresponding value from the domain.
Inversion is useful for interaction, say to determine the data value corresponding
to the position of the mouse.
-}
invert : Scale { a | invert : domain -> range -> value -> result, domain : domain, range : range } -> value -> result
invert (Scale scale) value =
    scale.invert scale.domain scale.range value


{-| Returns the extent of values in the domain for the corresponding value in the
range. This method is useful for interaction, say to determine the value in the
domain that corresponds to the pixel location under the mouse.
-}
invertExtent :
    Scale { a | invertExtent : domain -> range -> value -> Maybe ( comparable, comparable ), domain : domain, range : range }
    -> value
    -> Maybe ( comparable, comparable )
invertExtent (Scale scale) value =
    scale.invertExtent scale.domain scale.range value


{-| Retrieve the domain of the scale.
-}
domain : Scale { a | domain : domain } -> domain
domain (Scale scale) =
    scale.domain


{-| Retrieve the range of the scale.
-}
range : Scale { a | range : range } -> range
range (Scale options) =
    options.range


{-| Retrieve the minimum and maximum elements from the range.
-}
rangeExtent : Scale { a | rangeExtent : domain -> range -> ( b, b ), domain : domain, range : range } -> ( b, b )
rangeExtent (Scale options) =
    options.rangeExtent options.domain options.range



-- rangeRound : Scale { a | range : ( Float, Float ), rangeRound : ( Float, Float ) -> ( Float, Float ) } -> Scale { a | range : ( Float, Float ), rangeRound : ( Float, Float ) -> ( Float, Float ) }
-- rangeRound (Scale scale) =
--     Scale { scale | range = scale.rangeRound scale.range }


{-| The second argument controls approximately how many representative values from
the scale’s domain to return. A good default value 10. The returned tick values are uniformly spaced,
have human-readable values (such as multiples of powers of 10), and are guaranteed
to be within the extent of the domain. Ticks are often used to display reference
lines, or tick marks, in conjunction with the visualized data. The specified count
is only a hint; the scale may return more or fewer values depending on the domain.

    scale = linear ( 10, 100 ) ( 50, 100 )
    ticks scale 10 == [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]

-}
ticks : Scale { a | ticks : domain -> Int -> List ticks, domain : domain } -> Int -> List ticks
ticks (Scale scale) count =
    scale.ticks scale.domain count


{-| A number format function suitable for displaying a tick value, automatically
computing the appropriate precision based on the fixed interval between tick values.
The specified count should have the same value as the count that is used to generate the tick values.
-}
tickFormat : Scale { a | tickFormat : domain -> Int -> value -> String, domain : domain, convert : domain -> range -> value -> b } -> Int -> value -> String
tickFormat (Scale opts) =
    opts.tickFormat opts.domain



-- quantiles : Scale { a | quantiles : b } -> b
-- quantiles (Scale { quantiles }) =
--     quantiles


{-| Enables clamping on the domain, meaning the return value of the scale is
always within the scale’s range.

    scale = linear ( 10, 100 ) ( 50, 100 )
    convert scale 1 == 45
    convert (clamp scale) 1 == 50

-}
clamp : Scale { a | convert : ( Float, Float ) -> range -> Float -> result } -> Scale { a | convert : ( Float, Float ) -> range -> Float -> result }
clamp (Scale ({ convert } as scale)) =
    let
        convert_ ( mi, ma ) range value =
            convert ( mi, ma ) range <| Basics.clamp (min mi ma) (max mi ma) value
    in
        Scale { scale | convert = convert_ }


{-| Returns a new scale which extends the domain so that it lands on round values.
The second argument is the same as you would pass to ticks.

    scale = linear ( 0.5, 99 ) ( 50, 100 )
    domain (nice scale 10) == (0, 100)

-}
nice : Scale { a | nice : domain -> Int -> domain, domain : domain } -> Int -> Scale { a | nice : domain -> Int -> domain, domain : domain }
nice (Scale ({ nice, domain } as options)) count =
    Scale { options | domain = nice domain count }
