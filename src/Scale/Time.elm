module Scale.Time exposing (convert, invert, nice, rangeExtent, tickFormat, ticks)

import DateFormat
import Scale.Internal exposing (bimap, interpolateFloat)
import Scale.Linear as Linear
import Time
import Time.Extra exposing (Interval(..))


toTime ( a, b ) =
    ( Time.posixToMillis a |> toFloat, Time.posixToMillis b |> toFloat )


convert : ( Time.Posix, Time.Posix ) -> ( Float, Float ) -> Time.Posix -> Float
convert domain range =
    bimap (toTime domain) range (\d r v -> deinterpolate d r (Time.posixToMillis v |> toFloat)) interpolateFloat


invert : ( Time.Posix, Time.Posix ) -> ( Float, Float ) -> Float -> Time.Posix
invert domain range =
    bimap range (toTime domain) deinterpolate (\d r v -> Time.millisToPosix (round (interpolate d r v)))


rangeExtent : ( Time.Posix, Time.Posix ) -> ( Float, Float ) -> ( Float, Float )
rangeExtent d r =
    r


deinterpolate =
    Linear.deinterpolate


interpolate a b =
    interpolateFloat a b


ticks : Time.Zone -> ( Time.Posix, Time.Posix ) -> Int -> List Time.Posix
ticks zone domain count =
    let
        ( start, end ) =
            toTime domain

        target =
            abs (start - end) / toFloat count

        ( interval, step ) =
            findInterval target tickIntervals
    in
    Time.Extra.range interval (round step) zone (Tuple.first domain) (Tuple.second domain)


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


tickFormat : Time.Zone -> ( Time.Posix, Time.Posix ) -> Int -> Time.Posix -> String
tickFormat zone _ _ date =
    let
        time =
            Time.posixToMillis date

        significant interval =
            Time.posixToMillis (Time.Extra.floor interval zone date) < time

        format =
            if significant Second then
                [ DateFormat.text ".", DateFormat.millisecondFixed ]

            else if significant Minute then
                [ DateFormat.text ":", DateFormat.secondFixed ]

            else if significant Hour then
                [ DateFormat.hourFixed, DateFormat.text ":", DateFormat.minuteFixed ]

            else if significant Day then
                [ DateFormat.hourFixed, DateFormat.text " ", DateFormat.amPmLowercase ]

            else if significant Month then
                [ DateFormat.dayOfMonthFixed, DateFormat.text " ", DateFormat.monthNameAbbreviated ]

            else if significant Year then
                [ DateFormat.monthNameFull ]

            else
                [ DateFormat.yearNumber ]
    in
    DateFormat.format format zone date


nice : Time.Zone -> ( Time.Posix, Time.Posix ) -> Int -> ( Time.Posix, Time.Posix )
nice zone domain count =
    let
        ( start, end ) =
            toTime domain

        target =
            abs (start - end) / toFloat count

        ( interval, _ ) =
            findInterval target tickIntervals
    in
    ( Time.Extra.floor interval zone (Tuple.first domain), Time.Extra.ceiling interval zone (Tuple.second domain) )
