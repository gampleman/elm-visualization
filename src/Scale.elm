module Scale exposing
    ( Scale
    , ContinuousScale, linear, log, identity, time
    , SequentialScale, sequential
    , QuantizeScale, quantize
    , OrdinalScale, ordinal
    , BandScale, band, BandConfig, defaultBandConfig
    , convert, invert, invertExtent, domain, range, rangeExtent, ticks, tickFormat, clamp, nice, bandwidth, toRenderable
    -- , power
    )

{-| Scales are a convenient abstraction for a fundamental task in visualization:
mapping a dimension of abstract data to a visual representation. Although most
often used for position-encoding quantitative data, such as mapping a measurement
in meters to a position in pixels for dots in a scatterplot, scales can represent
virtually any visual encoding, such as diverging colors, stroke widths, or symbol
size. Scales can also be used with virtually any type of data, such as named
categorical data or discrete data that requires sensible breaks.

For [continuous](#ContinuousScale) quantitative data, you typically want a [linear scale](#linear). (For time
series data, a [time scale](#time).) If the distribution calls for it, consider
transforming data using a [log scale](#log). A [quantize scale](#QuantizeScale) may aid
differentiation by rounding continuous data to a fixed set of discrete values.

For discrete ordinal (ordered) or categorical (unordered) data, an [ordinal scale](#OrdinalScale)
specifies an explicit mapping from a set of data values to a corresponding set
of visual attributes (such as colors). The related [band](#BandScale) scale is
useful for position-encoding ordinal data, such as bars in a bar chart.

Scales have no intrinsic visual representation. However, most scales can generate
and format ticks for reference marks to aid in the construction of [axes](Axis).


### Scales

  - [Continuous](#ContinuousScale) ([linear](#linear), [log](#log), [identity](#identity), [time](#time))
  - [Sequential](#SequentialScale)
  - [Quantize](#QuantizeScale)
  - [Ordinal](#OrdinalScale) ([Band](#BandScale))

@docs Scale


# Continuous Scales

@docs ContinuousScale, linear, log, identity, time


# Sequential Scales

Sequential scales are similar to continuous scales in that they map a continuous,
numeric input domain to a continuous output range. However, unlike continuous
scales, the output range of a sequential scale is fixed by its interpolator function.

@docs SequentialScale, sequential

You can find some premade color interpolators in the [Scale.Color](Scale-Color) module.


# Quantize Scales

Quantize scales are similar to linear scales, except they use a discrete rather
than continuous range. The continuous input domain is divided into uniform
segments based on the number of values in (i.e., the cardinality of) the output
range. Each range value y can be expressed as a quantized linear function of the
domain value `x`: `y = m round(x) + b`.

@docs QuantizeScale, quantize


# Ordinal Scales

Unlike continuous scales, ordinal scales have a discrete domain and range. For
example, an ordinal scale might map a set of named categories to a set of colors,
or determine the horizontal positions of columns in a column chart.

@docs OrdinalScale, ordinal

You can find some premade color schemes in the [Scale.Color](Scale-Color) module.


# Band Scales

Band scales are like ordinal scales except the output range is continuous and
numeric. Discrete output values are automatically computed by the scale by
dividing the continuous range into uniform bands. Band scales are typically used
for bar charts with an ordinal or categorical dimension.

@docs BandScale, band, BandConfig, defaultBandConfig


# Operations

These functions take Scales and do something with them. Check the docs of each scale type to see which operations it supports.

@docs convert, invert, invertExtent, domain, range, rangeExtent, ticks, tickFormat, clamp, nice, bandwidth, toRenderable

-}

import Color exposing (Color)
import Scale.Band as Band
import Scale.Linear as Linear
import Scale.Log as Log
import Scale.Ordinal as Ordinal
import Scale.Quantize as Quantize
import Scale.Sequential as Sequential
import Scale.Time as TimeScale
import Time


{-| This API is highly polymorphic as each scale has different functions supported.
This is still done in a convenient and type-safe manner, however the cost is
a certain ugliness and complexity of the type signatures. For this reason after the type alias of each scale, the supported functions are listed along with a more specialized type signature appropriate for that scale type.

**Note:** As a convention, the scales typically take arguments in a `range -> domain` order. This may seem somewhat counterinutive, as scales map a domain onto a range, but it is quite common to need to compute the domain, but know the range statically, so this argument order works much better for composition.

If you're new to this, I recommend ignoring the types of the type aliases and of the operations and just look at these listings.

-}
type Scale scaleSpec
    = Scale scaleSpec



-- Continuous Scales


{-| Maps a `(inp, inp)` **domain** to a
`(Float, Float)` **range** (this will be either `(Float, Float)` or `(Time.Posix, Time.Posix)`.)

Continuous scales support the following operations:

  - [`convert : ContinuousScale inp -> inp -> Float`](#convert)
  - [`invert : ContinuousScale inp -> Float -> inp`](#invert)
  - [`domain : ContinuousScale inp -> (inp, inp)`](#domain)
  - [`range : ContinuousScale inp -> (Float, Float)`](#range)
  - [`rangeExtent : ContinuousScale inp -> (Float, Float)`](#rangeExtent) (which is in this case just an alias for `range`)
  - [`ticks : ContinuousScale inp -> Int -> List inp`](#ticks)
  - [`tickFormat : ContinuousScale inp -> Int -> inp -> String`](#tickFormat)
  - [`clamp : ContinuousScale inp -> ContinuousScale inp`](#clamp)
  - [`nice : Int -> ContinuousScale inp -> ContinuousScale inp`](#nice)

-}
type alias ContinuousScale inp =
    Scale
        { domain : ( inp, inp )
        , range : ( Float, Float )
        , convert : ( inp, inp ) -> ( Float, Float ) -> inp -> Float
        , invert : ( inp, inp ) -> ( Float, Float ) -> Float -> inp
        , ticks : ( inp, inp ) -> Int -> List inp
        , tickFormat : ( inp, inp ) -> Int -> inp -> String
        , nice : ( inp, inp ) -> Int -> ( inp, inp )
        , rangeExtent : ( inp, inp ) -> ( Float, Float ) -> ( Float, Float )
        }


{-| Linear scales are a good default choice for continuous quantitative data
because they preserve proportional differences. Each range value y can be
expressed as a function of the domain value x: y = mx + b.

    scale : ContinuousScale
    scale = Scale.linear ( 50, 100 ) ( 0, 1 )
    Scale.convert scale 0.5 --> 75

-}
linear : ( Float, Float ) -> ( Float, Float ) -> ContinuousScale Float
linear range_ domain_ =
    Scale
        { domain = domain_
        , range = range_
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
--     scale : ContinuousScale
--     scale = power 2 ( 0, 1 ) ( 50, 100 )
--     convert scale 0.5 == 62.5
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

The arguments are `base`, `range`, and `domain`.

    scale : ContinuousScale
    scale = log 10 ( 10, 1000 ) ( 50, 100 )
    convert scale 100 --> 75

-}
log : Float -> ( Float, Float ) -> ( Float, Float ) -> ContinuousScale Float
log base range_ domain_ =
    Scale
        { domain = domain_
        , range = range_
        , convert = Log.convert
        , invert = Log.invert
        , ticks = Log.ticks base
        , tickFormat = Log.tickFormat base
        , nice = Log.nice base
        , rangeExtent = Log.rangeExtent
        }


{-| Identity scales are a special case of linear scales where the domain and
range are identical; the convert and invert operations are thus the identity function.
These scales are occasionally useful when working with pixel coordinates, say in
conjunction with an axis.
-}
identity : ( Float, Float ) -> ContinuousScale Float
identity domainOrRange =
    linear domainOrRange domainOrRange


{-| Time scales are a variant of linear scales that have a temporal domain: domain
values are times rather than floats, and invert likewise returns a time.
Time scales implement ticks based on calendar intervals, taking the pain out of
generating axes for temporal domains.

Since time scales use human time to calculate ticks and display ticks, we need the
time zone that you will want to display your data in.

-}
time : Time.Zone -> ( Float, Float ) -> ( Time.Posix, Time.Posix ) -> ContinuousScale Time.Posix
time zone range_ domain_ =
    Scale
        { domain = domain_
        , range = range_
        , convert = TimeScale.convert
        , invert = TimeScale.invert
        , ticks = TimeScale.ticks zone
        , tickFormat = TimeScale.tickFormat zone
        , nice = TimeScale.nice zone
        , rangeExtent = TimeScale.rangeExtent
        }



-- Sequential Scales


{-| This transforms a continuous `(Float, Float)`
domain to an arbitrary range `a` defined by the interpolator function `Float -> a`, where the `Float` goes from 0 to 1.

Sequential scales support the following operations:

  - [`convert : SequentialScale a -> Float -> a`](#convert)
  - [`domain : SequentialScale a -> (Float, Float)`](#domain)
  - [`range : SequentialScale a -> Float -> a`](#range)

-}
type alias SequentialScale a =
    Scale
        { domain : ( Float, Float )
        , range : Float -> a
        , convert : ( Float, Float ) -> (Float -> a) -> Float -> a
        }


{-| Construct a sequential scale.
-}
sequential : (Float -> a) -> ( Float, Float ) -> SequentialScale a
sequential interpolator domain_ =
    Scale
        { domain = domain_
        , range = interpolator
        , convert = Sequential.convert
        }



-- Quantize Scales


{-| These transform a `(Float, Float)` domain
to an arbitrary non-empty list `(a, List a)`.

Quantize scales support the following operations:

  - [`convert : QuantizeScale a -> Float -> a`](#convert),
  - [`invertExtent : QuantizeScale a -> a -> Maybe (Float, Float)`](#invertExtent)
  - [`domain : QuantizeScale a -> (Float, Float)`](#domain)
  - [`range : QuantizeScale a -> (a, List a)`](#range),
  - [`rangeExtent : QuantizeScale a -> (a, a)`](#rangeExtent)
  - [`ticks : QuantizeScale a -> Int -> List Float`](#ticks)
  - [`tickFormat : QuantizeScale a -> Int -> Float -> String`](#tickFormat)
  - [`nice : Int -> QuantizeScale a -> QuantizeScale a`](#nice)
  - [`clamp : QuantizeScale a -> QuantizeScale a`](#clamp)

-}
type alias QuantizeScale a =
    Scale
        { domain : ( Float, Float )
        , range : ( a, List a )
        , convert : ( Float, Float ) -> ( a, List a ) -> Float -> a
        , invertExtent : ( Float, Float ) -> ( a, List a ) -> a -> Maybe ( Float, Float )
        , ticks : ( Float, Float ) -> ( a, List a ) -> Int -> List Float
        , tickFormat : ( Float, Float ) -> Int -> Float -> String
        , nice : ( Float, Float ) -> Int -> ( Float, Float )
        , rangeExtent : ( Float, Float ) -> ( a, List a ) -> ( a, a )
        }


{-| Constructs a new quantize scale. The range for these is a
non-empty list represented as a `(head, tail)` tuple.
-}
quantize : ( a, List a ) -> ( Float, Float ) -> QuantizeScale a
quantize range_ domain_ =
    Scale
        { domain = domain_
        , range = range_
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


{-| Type alias for ordinal scales. These transform an arbitrary
`List a` domain to an arbitrary list `List b`, where the mapping
is based on order.

Ordinal scales support the following operations:

  - [`convert : OrdinalScale a b -> a -> Maybe b`](#convert)

    Note that this returns a `Maybe` value in the case when you pass a value that isn't in the domain.

  - [`domain : OrdinalScale a b -> List a`](#domain)

  - [`range : OrdinalScale a b -> List b`](#range)

-}
type alias OrdinalScale a b =
    Scale
        { domain : List a
        , range : List b
        , convert : List a -> List b -> a -> Maybe b
        }


{-| Constructs an ordinal scale.
-}
ordinal : List b -> List a -> OrdinalScale a b
ordinal range_ domain_ =
    Scale
        { domain = domain_
        , range = range_
        , convert = Ordinal.convert
        }


{-| Type alias for a band scale. These transform an arbitrary `List a`
to a continous (Float, Float) by uniformely partitioning the range.

Band scales support the following operations:

  - [`convert : BandScale a -> a -> Float`](#convert)
  - [`domain : BandScale a -> List a`](#domain)
  - [`range : Bandscale a -> (Float, Float)`](#range)
  - [`bandwidth : Bandscale a -> Float`](#bandwidth)
  - [`toRenderable : (a -> String) -> BandScale a -> RenderableScale a`](#toRenderable)

-}
type alias BandScale a =
    Scale
        { domain : List a
        , range : ( Float, Float )
        , convert : List a -> ( Float, Float ) -> a -> Float
        , bandwidth : Float
        }


{-| Configuration options for deciding how bands are partioned,


### `.paddingInner : Float`

The inner padding determines the ratio (so the value must be in
the range [0, 1]) of the range that is reserved for blank space
between bands.


### `.paddingOuter : Float`

The outer padding determines the ratio (so the value must be in
the range [0, 1]) of the range that is reserved for blank space
before the first band and after the last band.


### `.align : Float`

The alignment determines how any leftover unused space in the range
is distributed. A value of 0.5 indicates that the leftover space
should be equally distributed before the first band and after the last
band; i.e., the bands should be centered within the range. A value
of 0 or 1 may be used to shift the bands to one side, say to position
them adjacent to an axis.

-}
type alias BandConfig =
    { paddingInner : Float
    , paddingOuter : Float
    , align : Float
    }


{-| Creates some reasonable defaults for a BandConfig:

    defaultBandConfig --> { paddingInner = 0.0, paddingOuter = 0.0, align = 0.5 }

-}
defaultBandConfig : BandConfig
defaultBandConfig =
    { paddingInner = 0.0, paddingOuter = 0.0, align = 0.5 }


{-| Constructs a band scale.
-}
band : BandConfig -> ( Float, Float ) -> List a -> BandScale a
band config range_ domain_ =
    Scale
        { domain = domain_
        , range = range_
        , convert = Band.convert config
        , bandwidth = Band.bandwidth config domain_ range_
        }


{-| Returns the width of a band in a band scale.

    scale : BandScale String
    scale = Scale.band Scale.defaultBandConfig (0, 120) ["a", "b", "c"]

    Scale.bandwidth scale --> 40

-}
bandwidth : Scale { scale | bandwidth : Float } -> Float
bandwidth (Scale scale) =
    scale.bandwidth


{-| This converts a BandScale into a [RenderableScale](Axis#RenderableScale)
suitable for rendering Axes. This has the same domain and range, but the convert output is shifted by half a `bandwidth`
in order for ticks and labels to align nicely.
-}
toRenderable :
    (a -> String)
    -> BandScale a
    ->
        Scale
            { ticks : List a -> Int -> List a
            , domain : List a
            , tickFormat : List a -> Int -> a -> String
            , convert : List a -> ( Float, Float ) -> a -> Float
            , range : ( Float, Float )
            , rangeExtent : List a -> ( Float, Float ) -> ( Float, Float )
            }
toRenderable toString (Scale scale) =
    Scale
        { ticks = \dmn _ -> dmn
        , domain = scale.domain
        , tickFormat = \_ _ -> toString
        , convert = \dmn rng value -> scale.convert dmn rng value + max (scale.bandwidth - 1) 0 / 2
        , range = scale.range
        , rangeExtent = \_ rng -> rng
        }



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
the scale’s domain to return. A good default value is 10. The returned tick values are uniformly spaced,
have human-readable values (such as multiples of powers of 10), and are guaranteed
to be within the extent of the domain. Ticks are often used to display reference
lines, or tick marks, in conjunction with the visualized data. The specified count
is only a hint; the scale may return more or fewer values depending on the domain.

    scale : ContinuousScale Float
    scale = linear ( 10, 100 ) ( 50, 100 )
    ticks scale 10 --> [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]

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

    scale : ContinuousScale Float
    scale = Scale.linear  ( 50, 100 ) ( 10, 100 )

    Scale.convert scale 1 --> 45

    Scale.convert (Scale.clamp scale) 1 --> 50

-}
clamp : Scale { a | convert : ( Float, Float ) -> range -> Float -> result } -> Scale { a | convert : ( Float, Float ) -> range -> Float -> result }
clamp (Scale scale) =
    let
        convert_ ( mi, ma ) range_ value =
            scale.convert ( mi, ma ) range_ <| Basics.clamp (min mi ma) (max mi ma) value
    in
    Scale { scale | convert = convert_ }


{-| Returns a new scale which extends the domain so that it lands on round values.
The first argument is the same as you would pass to ticks.

    scale : ContinuousScale Float
    scale = Scale.linear ( 0.5, 99 ) ( 50, 100 )
    Scale.domain (Scale.nice 10 scale) --> (0, 100)

-}
nice : Int -> Scale { a | nice : domain -> Int -> domain, domain : domain } -> Scale { a | nice : domain -> Int -> domain, domain : domain }
nice count (Scale scale) =
    Scale { scale | domain = scale.nice scale.domain count }
