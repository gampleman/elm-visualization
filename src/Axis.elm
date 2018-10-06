module Axis exposing
    ( RenderableScale, left, right, bottom, top
    , Attribute, ticks, tickFormat, tickCount, tickSizeInner, tickSizeOuter, tickPadding
    )

{-| The axis component renders human-readable reference marks for scales. This
alleviates one of the more tedious tasks in visualizing data.

Renders an Axis based on a [Scale](./Scale).

    view =
      svg []
        [ g [ class [ "axis" ], transform [ Translate 0 300 ] ]
          [ Axis.left [ tickCount 10] myScale
          ]
        [

Regardless of orientation, axes are always rendered at the origin. To change the
position of the axis with respect to the chart, specify a transform attribute on
the containing element.

@docs RenderableScale, left, right, bottom, top


### Customizing the axis

The elements created by the axis are considered part of its public API.
You can apply external stylesheets to
customize the axis appearance. An axis consists of a path element of class
“domain” representing the extent of the scale’s domain, followed by transformed
`g` elements of class “tick” representing each of the scale’s ticks. Each tick has
a `line` element to draw the tick line, and a `text` element for the tick label.
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

@docs Attribute, ticks, tickFormat, tickCount, tickSizeInner, tickSizeOuter, tickPadding

-}

import Scale exposing (Scale)
import Svg exposing (..)
import Svg.Attributes as Attrs exposing (..)


{-| -}
type Attribute data
    = Ticks (List data)
    | TickFormat (data -> String)
    | TickCount Int
    | TickSizeInner Float
    | TickSizeOuter Float
    | TickPadding Float


{-| Pass a list of ticks to be rendered explicitely. Defaults to `Scale.ticks`.
Useful when you want to render the data points as ticks.
-}
ticks : List data -> Attribute data
ticks =
    Ticks


{-| A formatting function for the tick marks. Defaults to `Scale.tickFormat`.
-}
tickFormat : (data -> String) -> Attribute data
tickFormat =
    TickFormat


{-| How many tickmarks to approximately generate. Defaults to 10.
-}
tickCount : Int -> Attribute data
tickCount =
    TickCount


{-| The inner tick size controls the length of the tick lines, offset from the native position of the axis.
Defaults to 6.
-}
tickSizeInner : Float -> Attribute data
tickSizeInner =
    TickSizeInner


{-| The outer tick size controls the length of the square ends of the domain path, offset from the native position of the axis. Thus, the “outer ticks” are not actually ticks but part of the domain path, and their position is determined by the associated scale’s domain extent. Thus, outer ticks may overlap with the first or last inner tick. An outer tick size of 0 suppresses the square ends of the domain path, instead producing a straight line. Defaults to 6.
-}
tickSizeOuter : Float -> Attribute data
tickSizeOuter =
    TickSizeOuter


{-| Padding controls the space between tick marks and tick labels. Defaults to 3.
-}
tickPadding : Float -> Attribute data
tickPadding =
    TickPadding


{-| Axes are rendered based on a [`Scale`](./Scale).

Currently only continuous (including time), quantize and band (via the `toRenderable` function) scales are supported.

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


computeOptions attrs scale =
    let
        ( opts, postList ) =
            List.foldl
                (\attr ( babyOpts, post ) ->
                    case attr of
                        TickCount val ->
                            ( { babyOpts | tickCount = val }, post )

                        TickSizeInner val ->
                            ( { babyOpts | tickSizeInner = val }, post )

                        TickSizeOuter val ->
                            ( { babyOpts | tickSizeOuter = val }, post )

                        TickPadding val ->
                            ( { babyOpts | tickPadding = val }, post )

                        _ ->
                            ( babyOpts, attr :: post )
                )
                ( { tickCount = 10
                  , tickSizeInner = 6
                  , tickSizeOuter = 6
                  , tickPadding = 3
                  }
                , []
                )
                attrs
    in
    List.foldl
        (\attr options ->
            case attr of
                Ticks val ->
                    { options | ticks = val }

                TickFormat val ->
                    { options | tickFormat = val }

                _ ->
                    options
        )
        { ticks = Scale.ticks scale opts.tickCount
        , tickFormat = Scale.tickFormat scale opts.tickCount
        , tickCount = opts.tickCount
        , tickSizeInner = opts.tickSizeInner
        , tickSizeOuter = opts.tickSizeOuter
        , tickPadding = opts.tickPadding
        }
        postList


horizontalAttrs =
    { x = Attrs.x << String.fromFloat
    , y = Attrs.y << String.fromFloat
    , x1 = Attrs.x1 << String.fromFloat
    , x2 = Attrs.x2 << String.fromFloat
    , y1 = Attrs.y1 << String.fromFloat
    , y2 = Attrs.y2 << String.fromFloat
    , translate =
        \y ->
            "translate(0, " ++ String.fromFloat y ++ ")"
    , horizontal = True
    }


verticalAttrs =
    { x = Attrs.y << String.fromFloat
    , y = Attrs.x << String.fromFloat
    , x1 = Attrs.y1 << String.fromFloat
    , x2 = Attrs.y2 << String.fromFloat
    , y1 = Attrs.x1 << String.fromFloat
    , y2 = Attrs.x2 << String.fromFloat
    , translate =
        \x ->
            "translate(" ++ String.fromFloat x ++ ", 0)"
    , horizontal = False
    }


{-| A left oriented axis. In this orientation, ticks are drawn to the left of the vertical domain path.
-}
left : List (Attribute value) -> RenderableScale a domain range value -> Svg msg
left =
    element horizontalAttrs -1 "0.32em" "end"


{-| A right oriented axis. In this orientation, ticks are drawn to the right of the vertical domain path.
-}
right : List (Attribute value) -> RenderableScale a domain range value -> Svg msg
right =
    element horizontalAttrs 1 "0.32em" "start"


{-| A top oriented axis. In this orientation, ticks are drawn above the horizontal domain path.
-}
top : List (Attribute value) -> RenderableScale a domain range value -> Svg msg
top =
    element verticalAttrs -1 "0em" "middle"


{-| A bottom oriented axis. In this orientation, ticks are drawn below the horizontal domain path.
-}
bottom : List (Attribute value) -> RenderableScale a domain range value -> Svg msg
bottom =
    element verticalAttrs 1 "0.71em" "middle"


element { x, y, x1, x2, y1, y2, translate, horizontal } k displacement textAnchorPosition =
    \attrs scale ->
        let
            opts =
                computeOptions attrs scale

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

            drawTick tick =
                g [ class "tick", transform (translate (position tick)) ]
                    [ line [ stroke "#000", x2 (k * opts.tickSizeInner), y1 0.5, y2 0.5 ] []
                    , text_ [ fill "#000", x (k * spacing), y 0.5, dy displacement ] [ text (opts.tickFormat tick) ]
                    ]

            domainLine =
                if horizontal then
                    "M" ++ String.fromFloat (k * opts.tickSizeOuter) ++ "," ++ String.fromFloat range0 ++ "H0.5V" ++ String.fromFloat range1 ++ "H" ++ String.fromFloat (k * opts.tickSizeOuter)

                else
                    "M" ++ String.fromFloat range0 ++ "," ++ String.fromFloat (k * opts.tickSizeOuter) ++ "V0.5H" ++ String.fromFloat range1 ++ "V" ++ String.fromFloat (k * opts.tickSizeOuter)
        in
        g [ fill "none", fontSize "10", fontFamily "sans-serif", textAnchor textAnchorPosition ]
            (Svg.path [ class "domain", stroke "#000", d domainLine ] []
                :: List.map drawTick opts.ticks
            )
