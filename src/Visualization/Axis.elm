module Visualization.Axis exposing (..)

import Visualization.Scale as Scale exposing (Scale)
import Svg exposing (..)
import Svg.Attributes as Attrs exposing (..)


type Orientation
    = Left
    | Right
    | Top
    | Bottom


type alias Options a =
    { orientation : Orientation
    , ticks : Maybe (List a)
    , tickFormat : Maybe (a -> String)
    , tickCount : Int
    , tickSizeInner : Float
    , tickSizeOuter : Float
    , tickPadding : Float
    }


defaultOptions : Options a
defaultOptions =
    { orientation = Left
    , ticks = Nothing
    , tickFormat = Nothing
    , tickCount = 10
    , tickSizeInner = 6
    , tickSizeOuter = 6
    , tickPadding = 3
    }


type alias RenderableScale a domain range value =
    Scale
        { a
            | ticks : domain -> Int -> List value
            , domain : domain
            , tickFormat : domain -> Int -> value -> String
            , convert : domain -> range -> value -> Float
            , range : range
            , rangeExtent : domain -> range -> ( Float, Float )
        }


axis : Options value -> RenderableScale a domain range value -> Svg msg
axis opts scale =
    let
        ticks =
            Maybe.withDefault (Scale.ticks scale opts.tickCount) opts.ticks

        format =
            Maybe.withDefault (Scale.tickFormat scale opts.tickCount) opts.tickFormat

        spacing =
            Basics.max opts.tickSizeInner 0 + opts.tickPadding

        rangeExtent =
            Scale.rangeExtent scale

        range0 =
            fst rangeExtent + 0.5

        range1 =
            snd rangeExtent + 0.5

        position =
            Scale.convert scale

        horizontalAttrs =
            ( Attrs.x << toString, Attrs.y << toString, Attrs.x1 << toString, Attrs.x2 << toString, Attrs.y1 << toString, Attrs.y2 << toString )

        verticalAttrs =
            ( Attrs.y << toString, Attrs.x << toString, Attrs.y1 << toString, Attrs.y2 << toString, Attrs.x1 << toString, Attrs.x2 << toString )

        translateX point =
            "translate(" ++ toString (position point) ++ ", 0)"

        translateY point =
            "translate(0, " ++ toString (position point) ++ ")"

        ( k, dy', textAnchorPosition, translate, ( x, y, x1, x2, y1, y2 ) ) =
            case opts.orientation of
                Left ->
                    ( -1, "0.32em", "end", translateY, horizontalAttrs )

                Top ->
                    ( -1, "0em", "middle", translateX, verticalAttrs )

                Right ->
                    ( 1, "0.32em", "start", translateY, horizontalAttrs )

                Bottom ->
                    ( 1, "0.71em", "middle", translateX, verticalAttrs )

        drawTick tick =
            g [ class "tick", transform (translate tick) ]
                [ line [ stroke "#000", x2 (k * opts.tickSizeInner), y1 0.5, y2 0.5 ] []
                , text' [ fill "#000", x (k * spacing), y 0.5, dy dy' ] [ text (format tick) ]
                ]

        domainLine =
            if opts.orientation == Left || opts.orientation == Right then
                "M" ++ toString (k * opts.tickSizeOuter) ++ "," ++ toString range0 ++ "H0.5V" ++ toString range1 ++ "H" ++ toString (k * opts.tickSizeOuter)
            else
                "M" ++ toString range0 ++ "," ++ toString (k * opts.tickSizeOuter) ++ "V0.5H" ++ toString range1 ++ "V" ++ toString (k * opts.tickSizeOuter)
    in
        g [ fill "none", fontSize "10", fontFamily "sans-serif", textAnchor textAnchorPosition ]
            (Svg.path [ class "domain", stroke "#000", d domainLine ] []
                :: List.map drawTick ticks
            )
