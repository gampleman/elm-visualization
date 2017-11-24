module Visualization.Scale.Time exposing (convert, invert, ticks, tickFormat, nice, rangeExtent)

import Date.Extra as Date exposing (range, Interval(..))
import Date exposing (Date)
import Visualization.Scale.Internal exposing (bimap, interpolateFloat)
import Visualization.Scale.Linear as Linear


toTime ( a, b ) =
    ( Date.toTime a, Date.toTime b )


convert : ( Date, Date ) -> ( Float, Float ) -> Date -> Float
convert domain range =
    bimap (toTime domain) range (\d r v -> deinterpolate d r (Date.toTime v)) interpolateFloat


invert : ( Date, Date ) -> ( Float, Float ) -> Float -> Date
invert domain range =
    bimap range (toTime domain) deinterpolate (\d r v -> Date.fromTime (interpolate d r v))


rangeExtent : ( Date, Date ) -> ( Float, Float ) -> ( Float, Float )
rangeExtent d r =
    r


deinterpolate =
    Linear.deinterpolate


interpolate a b =
    interpolateFloat a b


ticks : ( Date, Date ) -> Int -> List Date
ticks domain count =
    let
        ( start, end ) =
            toTime domain

        target =
            abs (start - end) / (toFloat count)

        ( interval, step ) =
            findInterval target tickIntervals
    in
        Date.range interval (round step) (Date.fromTime start) (Date.fromTime end)


tickIntervals =
    [ ( Second, 1 )
    , ( Second, 5 )
    , ( Second, 15 )
    , ( Second, 30 )
    , ( Minute, 1 )
    , ( Minute, 5 )
    , ( Minute, 15 )
    , ( Minute, 30 )
    , ( Hour, 1 )
    , ( Hour, 3 )
    , ( Hour, 6 )
    , ( Hour, 12 )
    , ( Day, 1 )
    , ( Day, 2 )
    , ( Week, 1 )
    , ( Month, 1 )
    , ( Month, 3 )
    , ( Year, 1 )
    ]


timeLength interval =
    case interval of
        Millisecond ->
            1

        Second ->
            1000

        Minute ->
            60 * 1000

        Hour ->
            60 * 60 * 1000

        Day ->
            24 * 60 * 60 * 1000

        Month ->
            30 * 24 * 60 * 60 * 1000

        Year ->
            365 * 30 * 24 * 60 * 60 * 1000

        Quarter ->
            4 * 30 * 24 * 60 * 60 * 1000

        Week ->
            7 * 24 * 60 * 60 * 1000

        _ ->
            0


findInterval target intervals =
    case intervals of
        [] ->
            ( Year, 1 )

        ( interval, step ) :: ( interval_, step_ ) :: xs ->
            let
                ratio =
                    target / (step * timeLength interval)

                ratio_ =
                    (step_ * timeLength interval_) / target
            in
                if ratio < ratio_ then
                    ( interval, step )
                else
                    findInterval target (( interval_, step_ ) :: xs)

        x :: xs ->
            x


tickFormat : ( Date, Date ) -> Int -> Date -> String
tickFormat _ _ date =
    let
        time =
            Date.toTime date

        significant interval =
            Date.toTime (Date.floor interval date) < time
    in
        if significant Second then
            Date.toFormattedString ".SSS" date
        else if significant Minute then
            Date.toFormattedString ":ss" date
        else if significant Hour then
            Date.toFormattedString "hh:mm" date
        else if significant Day then
            Date.toFormattedString "hh a" date
        else if significant Month then
            Date.toFormattedString "dd MMM" date
        else if significant Year then
            Date.toFormattedString "MMMM" date
        else
            Date.toFormattedString "yyyy" date


nice : ( Date, Date ) -> Int -> ( Date, Date )
nice domain count =
    let
        ( start, end ) =
            toTime domain

        target =
            abs (start - end) / (toFloat count)

        ( interval, _ ) =
            findInterval target tickIntervals
    in
        ( Date.floor interval (Date.fromTime start), Date.ceiling interval (Date.fromTime end) )
