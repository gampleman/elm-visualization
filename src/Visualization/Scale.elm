module Visualization.Scale exposing (..)

import Date exposing (Date)
import Visualization.Scale.Linear as Linear
import Visualization.Scale.Log as Log
import Visualization.Scale.Time as Time
import Visualization.Scale.Sequential as Sequential
import Visualization.Scale.Quantize as Quantize


type Scale scaleSpec
    = Scale scaleSpec



-- Continous Scales


type alias ContinousScale =
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


linear : ( Float, Float ) -> ( Float, Float ) -> ContinousScale
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


power : Float -> ( Float, Float ) -> ( Float, Float ) -> ContinousScale
power exponent =
    Debug.crash "not implemented"


log : Float -> ( Float, Float ) -> ( Float, Float ) -> ContinousScale
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


identity : ContinousScale
identity =
    linear ( 0, 1 ) ( 0, 1 )


type alias ContinousTimeScale =
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


time : ( Date, Date ) -> ( Float, Float ) -> ContinousTimeScale
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


type alias SequentialScale a =
    Scale
        { domain : ( Float, Float )
        , range : Float -> a
        , convert : ( Float, Float ) -> (Float -> a) -> Float -> a
        }


sequential : ( Float, Float ) -> (Float -> a) -> SequentialScale a
sequential domain interpolator =
    Scale
        { domain = domain
        , range = interpolator
        , convert = Sequential.convert
        }



-- Quantize Scales


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


convert : Scale { a | convert : domain -> range -> value -> result, domain : domain, range : range } -> value -> result
convert (Scale scale) value =
    scale.convert scale.domain scale.range value


invert : Scale { a | invert : domain -> range -> value -> result, domain : domain, range : range } -> value -> result
invert (Scale scale) value =
    scale.invert scale.domain scale.range value


invertExtent :
    Scale { a | invertExtent : domain -> range -> value -> Maybe ( comparable, comparable ), domain : domain, range : range }
    -> value
    -> Maybe ( comparable, comparable )
invertExtent (Scale scale) value =
    scale.invertExtent scale.domain scale.range value


domain : Scale { a | domain : domain } -> domain
domain (Scale scale) =
    scale.domain


range : Scale { a | range : range } -> range
range (Scale options) =
    options.range


rangeExtent : Scale { a | rangeExtent : domain -> range -> ( b, b ), domain : domain, range : range } -> ( b, b )
rangeExtent (Scale options) =
    options.rangeExtent options.domain options.range


rangeRound : Scale { a | range : ( Float, Float ), rangeRound : ( Float, Float ) -> ( Float, Float ) } -> Scale { a | range : ( Float, Float ), rangeRound : ( Float, Float ) -> ( Float, Float ) }
rangeRound (Scale scale) =
    Scale { scale | range = scale.rangeRound scale.range }


ticks : Scale { a | ticks : domain -> Int -> List ticks, domain : domain } -> Int -> List ticks
ticks (Scale scale) count =
    scale.ticks scale.domain count


tickFormat : Scale { a | tickFormat : domain -> Int -> value -> String, domain : domain, convert : domain -> range -> value -> b } -> Int -> value -> String
tickFormat (Scale opts) =
    opts.tickFormat opts.domain


quantiles : Scale { a | quantiles : b } -> b
quantiles (Scale { quantiles }) =
    quantiles


clamp : Scale { a | convert : ( Float, Float ) -> range -> Float -> result } -> Scale { a | convert : ( Float, Float ) -> range -> Float -> result }
clamp (Scale ({ convert } as scale)) =
    let
        convert' ( mi, ma ) range value =
            convert ( mi, ma ) range <| Basics.clamp (min mi ma) (max mi ma) value
    in
        Scale { scale | convert = convert' }


nice : Scale { a | nice : domain -> Int -> domain, domain : domain } -> Int -> Scale { a | nice : domain -> Int -> domain, domain : domain }
nice (Scale ({ nice, domain } as options)) count =
    Scale { options | domain = nice domain count }
