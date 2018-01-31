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
        , infernoInterpolator
        , magmaInterpolator
        , plasmaInterpolator
        , OrdinalScale
        , ordinal
        , BandScale
        , band
        , bandwidth
        , defaultBandConfig
        , category10
        , category20a
        , category20b
        , category20c
        , BandConfig
        , toRenderable
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

@docs SequentialScale, sequential

Sequential scales support the following operations:

@docs convert, domain, rangeExtent


### Interpolator functions

Here are a few pre-built interpolator functions you can use with sequential scales:

@docs viridisInterpolator, infernoInterpolator, magmaInterpolator, plasmaInterpolator


# Quantize Scales

Quantize scales are similar to linear scales, except they use a discrete rather
than continuous range. The continuous input domain is divided into uniform
segments based on the number of values in (i.e., the cardinality of) the output
range. Each range value y can be expressed as a quantized linear function of the
domain value `x`: `y = m round(x) + b`.

@docs QuantizeScale, quantize

Quantize scales support the following operations:

@docs convert, invertExtent, domain, range, rangeExtent, ticks, tickFormat, nice


# Ordinal Scales

Unlike continuous scales, ordinal scales have a discrete domain and range. For
example, an ordinal scale might map a set of named categories to a set of colors,
or determine the horizontal positions of columns in a column chart.

@docs OrdinalScale, ordinal

Ordinal scales support the following operations:

@docs convert

Note that convert returns a Maybe for Ordinal scales. It is up to you to handle
potentially missing values in the domain.

@docs domain, range

Here are a few color schemes that you can use with ordinal scales to
support categorical data:

@docs category10, category20a, category20b, category20c


# Band Scales

Band scales are like ordinal scales except the output range is continuous and
numeric. Discrete output values are automatically computed by the scale by
dividing the continuous range into uniform bands. Band scales are typically used
for bar charts with an ordinal or categorical dimension.

@docs BandScale, band, BandConfig, defaultBandConfig

Band scales support the following operations:

@docs convert, domain, range, bandwidth, toRenderable

-}

import Color exposing (Color)
import Date exposing (Date)
import Visualization.Scale.Band as Band
import Visualization.Scale.Colors as Colors
import Visualization.Scale.Linear as Linear
import Visualization.Scale.Log as Log
import Visualization.Scale.Ordinal as Ordinal
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

    scale : ContinuousScale
    scale = linear ( 0, 1 ) ( 50, 100 )
    convert scale 0.5 --> 75

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

The arguments are `base`, `domain` and `range`.

    scale : ContinuousScale
    scale = log 10 ( 10, 1000 ) ( 50, 100 )
    convert scale 100 --> 75

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


{-| Type alias for sequential scales. This transforms a continuous `(Float, Float)`
domain to an arbitrary range `a` defined by the interpolator function `Float -> a`.
-}
type alias SequentialScale a =
    Scale
        { domain : ( Float, Float )
        , range : Float -> a
        , convert : ( Float, Float ) -> (Float -> a) -> Float -> a
        }


{-| Construct a sequential scale.
-}
sequential : ( Float, Float ) -> (Float -> a) -> SequentialScale a
sequential domain interpolator =
    Scale
        { domain = domain
        , range = interpolator
        , convert = Sequential.convert
        }


{-| ![Viridis](http://code.gampleman.eu/elm-visualization/misc/viridis.png)

Given a number t in the range [0,1], returns the corresponding
color from the “viridis” perceptually-uniform color scheme designed
by [van der Walt, Smith and Firing](https://bids.github.io/colormap/)
for matplotlib, represented as a core Color value.

-}
viridisInterpolator : Float -> Color
viridisInterpolator =
    Colors.viridis


{-| ![Inferno](http://code.gampleman.eu/elm-visualization/misc/inferno.png)

Given a number t in the range [0,1], returns the corresponding
color from the “inferno” perceptually-uniform color scheme designed
by [van der Walt, Smith and Firing](https://bids.github.io/colormap/)
for matplotlib, represented as a core Color value.

-}
infernoInterpolator : Float -> Color
infernoInterpolator =
    Colors.inferno


{-| ![magma](http://code.gampleman.eu/elm-visualization/misc/magma.png)

Given a number t in the range [0,1], returns the corresponding
color from the “magma” perceptually-uniform color scheme designed
by [van der Walt, Smith and Firing](https://bids.github.io/colormap/)
for matplotlib, represented as a core Color value.

-}
magmaInterpolator : Float -> Color
magmaInterpolator =
    Colors.magma


{-| ![Plasma](http://code.gampleman.eu/elm-visualization/misc/plasma.png)

Given a number t in the range [0,1], returns the corresponding
color from the “plasma” perceptually-uniform color scheme designed
by [van der Walt, Smith and Firing](https://bids.github.io/colormap/)
for matplotlib, represented as a core Color value.

-}
plasmaInterpolator : Float -> Color
plasmaInterpolator =
    Colors.plasma



-- Quantize Scales


{-| Type alias for quantize scales. These transform a `(Float, Float)` domain
to an arbitrary non-empty list `(a, List a)`.
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


{-| Constructs a new quantize scale. The range for these is a
non-empty list represented as a `(head, tail)` tuple.
-}
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


{-| Type alias for ordinal scales. These transform an arbitrary
`List a` domain to an arbitrary list `List b`, where the mapping
is based on order.
-}
type alias OrdinalScale a b =
    Scale
        { domain : List a
        , range : List b
        , convert : List a -> List b -> a -> Maybe b
        }


type alias ImplicitOrdinalScale a b =
    Scale
        { domain : List a
        , range : ( b, List b )
        , convert : List a -> ( b, List b ) -> a -> ( b, OrdinalScale a b )
        }


ordinalImplicit : List a -> ( b, List b ) -> ImplicitOrdinalScale a b
ordinalImplicit domain range =
    Scale
        { domain = domain
        , range = range
        , convert = \a -> Debug.crash "not implemented"
        }


{-| Constructs an ordinal scale.
-}
ordinal : List a -> List b -> OrdinalScale a b
ordinal domain range =
    Scale
        { domain = domain
        , range = range
        , convert = Ordinal.convert
        }


{-| Type alias for a band scale. These transform an arbitrary `List a`
to a continous (Float, Float) by uniformely partitioning the range.
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
band : BandConfig -> List a -> ( Float, Float ) -> BandScale a
band config domain range =
    Scale
        { domain = domain
        , range = range
        , convert = Band.convert config
        , bandwidth = Band.bandwidth config domain range
        }


{-| Returns the width of a band in a band scale.

    scale : BandScale String
    scale = band defaultBandConfig ["a", "b", "c"] (0, 120)

    bandwidth scale --> 40

-}
bandwidth : Scale { scale | bandwidth : Float } -> Float
bandwidth (Scale { bandwidth }) =
    bandwidth


{-| This converts a BandScale into a [RenderableScale](http://package.elm-lang.org/packages/gampleman/elm-visualization/latest/Visualization-Axis#RenderableScale)
suitable for rendering Axes. This has the same domain and range, but the convert output is shifted by half a `bandwidth`
in order for ticks and labels to align nicely.
-}
toRenderable :
    BandScale a
    ->
        Scale
            { ticks : List a -> Int -> List a
            , domain : List a
            , tickFormat : List a -> Int -> a -> String
            , convert : List a -> ( Float, Float ) -> a -> Float
            , range : ( Float, Float )
            , rangeExtent : List a -> ( Float, Float ) -> ( Float, Float )
            }
toRenderable (Scale { domain, range, convert, bandwidth }) =
    Scale
        { ticks = \domain _ -> domain
        , domain = domain
        , tickFormat = \_ _ -> toString
        , convert = \domain range value -> convert domain range value + max (bandwidth - 1) 0 / 2
        , range = range
        , rangeExtent = \_ range -> range
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
the scale’s domain to return. A good default value 10. The returned tick values are uniformly spaced,
have human-readable values (such as multiples of powers of 10), and are guaranteed
to be within the extent of the domain. Ticks are often used to display reference
lines, or tick marks, in conjunction with the visualized data. The specified count
is only a hint; the scale may return more or fewer values depending on the domain.

    scale : ContinuousScale
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

    scale : ContinuousScale
    scale = linear ( 10, 100 ) ( 50, 100 )

    convert scale 1 --> 45

    convert (Visualization.Scale.clamp scale) 1 --> 50

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

    scale : ContinuousScale
    scale = linear ( 0.5, 99 ) ( 50, 100 )
    domain (nice scale 10) --> (0, 100)

-}
nice : Scale { a | nice : domain -> Int -> domain, domain : domain } -> Int -> Scale { a | nice : domain -> Int -> domain, domain : domain }
nice (Scale ({ nice, domain } as options)) count =
    Scale { options | domain = nice domain count }


{-| ![category10](http://code.gampleman.eu/elm-visualization/misc/category10.png)

A list of ten categorical colors

-}
category10 : List Color
category10 =
    Colors.cat10


{-| ![category20a](http://code.gampleman.eu/elm-visualization/misc/category20a.png)

A list of twenty categorical colors

-}
category20a : List Color
category20a =
    Colors.cat20a


{-| ![category20b](http://code.gampleman.eu/elm-visualization/misc/category20b.png)

A list of twenty categorical colors

-}
category20b : List Color
category20b =
    Colors.cat20b


{-| ![category20c](http://code.gampleman.eu/elm-visualization/misc/category20c.png)

A list of twenty categorical colors. This color scale includes color
specifications and designs developed by Cynthia Brewer (colorbrewer2.org).

-}
category20c : List Color
category20c =
    Colors.cat20c
