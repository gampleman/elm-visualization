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
convert (Scale { convert, domain, range }) value =
    convert domain range value


invert : Scale { a | invert : domain -> range -> value -> result, domain : domain, range : range } -> value -> result
invert (Scale { invert, domain, range }) value =
    invert domain range value


invertExtent :
    Scale { a | invertExtent : domain -> range -> value -> Maybe ( comparable, comparable ), domain : domain, range : range }
    -> value
    -> Maybe ( comparable, comparable )
invertExtent (Scale { invertExtent, domain, range }) value =
    invertExtent domain range value


domain : Scale { a | domain : domain } -> domain
domain (Scale { domain }) =
    domain


rangeRound : Scale { a | range : ( Float, Float ), rangeRound : ( Float, Float ) -> ( Float, Float ) } -> Scale { a | range : ( Float, Float ), rangeRound : ( Float, Float ) -> ( Float, Float ) }
rangeRound (Scale ({ range, rangeRound } as options)) =
    Scale { options | range = rangeRound range }


ticks : Scale { a | ticks : domain -> Int -> List ticks, domain : domain } -> Int -> List ticks
ticks (Scale { domain, ticks }) count =
    ticks domain count


tickFormat : Scale { a | tickFormat : domain -> Int -> value -> String, domain : domain, convert : domain -> range -> b -> value } -> Int -> value -> String
tickFormat (Scale { tickFormat, domain }) =
    tickFormat domain


quantiles : Scale { a | quantiles : b } -> b
quantiles (Scale { quantiles }) =
    quantiles


clamp : Scale { a | convert : ( Float, Float ) -> range -> Float -> result } -> Scale { a | convert : ( Float, Float ) -> range -> Float -> result }
clamp (Scale ({ convert } as options)) =
    let
        convert' ( mi, ma ) range value =
            convert ( mi, ma ) range <| Basics.clamp (min mi ma) (max mi ma) value
    in
        Scale { options | convert = convert' }


nice : Scale { a | nice : domain -> Int -> domain, domain : domain } -> Int -> Scale { a | nice : domain -> Int -> domain, domain : domain }
nice (Scale ({ nice, domain } as options)) count =
    Scale { options | domain = nice domain count }
