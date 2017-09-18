module Visualization.Axis exposing (..)

{-| The axis component renders human-readable reference marks for scales. This
alleviates one of the more tedious tasks in visualizing data.

@docs axis, defaultOptions, Options, Orientation, RenderableScale

-}

import Visualization.Scale as Scale exposing (Scale)
import Svg exposing (..)
import Svg.Attributes as Attrs exposing (..)


{-| -}
type Orientation
    = Left
    | Right
    | Top
    | Bottom


{-| Options for configuring the scale:

  - `orientation`: Where to render the Axis.
  - `ticks`: Optionally pass ticks (in the domain). Defaults to `Scale.ticks`.
  - `tickFormat`: A formatting function for the tick marks. Defaults to `Scale.tickFormat`.
  - `tickCount`: How many tickmarks to approximately generate. Defaults to 10.

-}
type alias Options a =
    { orientation : Orientation
    , ticks : Maybe (List a)
    , tickFormat : Maybe (a -> String)
    , tickCount : Int
    , tickSizeInner : Float
    , tickSizeOuter : Float
    , tickPadding : Float
    }


{-| Default options to use
-}
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


{-| A type alias for the scale. Currently only continuous (including time) and band (via the `toRenderable` function) scales are supported.
-}
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


{-| Renders an Axis based on a [Scale](Visualization-Scale).

    view =
      svg []
        [ g [class "axis", transform "translate(0, 300)"]
          [ Axis.axis Axis.defaultOptions myScale
          ]
        [

Regardless of orientation, axes are always rendered at the origin. To change the
position of the axis with respect to the chart, specify a transform attribute on
the containing element.

The elements created by the axis are considered part of its public API.
You can apply external stylesheets to
customize the axis appearance. An axis consists of a path element of class
“domain” representing the extent of the scale’s domain, followed by transformed
g elements of class “tick” representing each of the scale’s ticks. Each tick has
a line element to draw the tick line, and a text element for the tick label.
For example, here is a typical bottom-oriented axis:

    <g fill="none" font-size="10" font-family="sans-serif" text-anchor="middle">
      <path class="domain" stroke="#000" d="M0.5,6V0.5H880.5V6"></path>
      <g class="tick" opacity="1" transform="translate(0,0)">
        <line stroke="#000" y2="6" x1="0.5" x2="0.5"></line>
        <text fill="#000" y="9" x="0.5" dy="0.71em">0.0</text>
      </g>
      <g class="tick" opacity="1" transform="translate(176,0)">
        <line stroke="#000" y2="6" x1="0.5" x2="0.5"></line>
        <text fill="#000" y="9" x="0.5" dy="0.71em">0.2</text>
      </g>
      <g class="tick" opacity="1" transform="translate(352,0)">
        <line stroke="#000" y2="6" x1="0.5" x2="0.5"></line>
        <text fill="#000" y="9" x="0.5" dy="0.71em">0.4</text>
      </g>
      <g class="tick" opacity="1" transform="translate(528,0)">
        <line stroke="#000" y2="6" x1="0.5" x2="0.5"></line>
        <text fill="#000" y="9" x="0.5" dy="0.71em">0.6</text>
      </g>
      <g class="tick" opacity="1" transform="translate(704,0)">
        <line stroke="#000" y2="6" x1="0.5" x2="0.5"></line>
        <text fill="#000" y="9" x="0.5" dy="0.71em">0.8</text>
      </g>
      <g class="tick" opacity="1" transform="translate(880,0)">
        <line stroke="#000" y2="6" x1="0.5" x2="0.5"></line>
        <text fill="#000" y="9" x="0.5" dy="0.71em">1.0</text>
      </g>
    </g>

-}
axis :
    Options value
    -> RenderableScale a domain range value
    -> Svg msg
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
            Tuple.first rangeExtent + 0.5

        range1 =
            Tuple.second rangeExtent + 0.5

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

        ( k, dy_, textAnchorPosition, translate, ( x, y, x1, x2, y1, y2 ) ) =
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
                , text_ [ fill "#000", x (k * spacing), y 0.5, dy dy_ ] [ text (format tick) ]
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
