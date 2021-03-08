# elm-visualization - An Introduction

By Jakub Hampl (@gampleman)

Welcome to elm-visualization! In this introduction I'll briefly explain the concepts behind elm-visualization, some of the prerequisite knowledge. I'll also point you to some learning resources.

The best way to understand elm-visualization is as a standard library for data visualization. It aims to provide all the basic tools you'll need to build any data visualization you might imagine. It is not a framework in the sense that it doesn't provide any structure for you to base your code on, nor is it merely a configurable collection of pre-made charts. It is also decidedly not very high level in its approach. This leads to a distinct set of properties:

1. You control and understand your visualization code. This means that you can have confidence that as requirements (invariably) change, you will not hit a wall where suddenly something is "impossible" because the library doesn't support it out of the box.
2. Simultaneously you have much more granular control over the performance characteristics of your code.
3. You are forced to make more explicit decisions about the graphical style and user affordances. You won't have features just because they were the defaults in the library.
4. Making decisions usually leads to deeper insight into the properties of the data and can lead to more meaningful designs.
5. Making a simple visualization will lead to a relatively large amount of code. Making an unusual or complex visualization won't increase that code that much.
6. Huge flexibility. elm-visualization focuses on composable primitives, so there is little limit on what you can do.

Given these properties, you should consider whether elm-visualization is the right tool for the job. I think it excels at building novel charts, at building charts for "production" where you understand the data being visualized and for data journalism/storytelling. In other words:

- If you are trying to do **expoloratory** visualization (i.e. you don't know much about the data), I would recommend either using a tool (like Excel or Tablaux) or using a Grammar of Graphics style library like [gicentre/elm-vegalite](https://package.elm-lang.org/packages/gicentre/elm-vegalite/latest/).
- If you need some specific **well known chart type**, you might have an easier time just using a pre-made package. See the [Elm Package Catalog Visualization Page](https://korban.net/elm/catalog/packages/data/visualisation) to see what's available.

## Prerequsites

Elm-visualization uses a low-level approach. Unfortunately, that means there is quite a lot to learn.

- Most importantly, elm-visualization does very little actual rendering for you. You will have to write your own drawing code. The easiest way to do that in today's Elm is to use SVG. Unfortunately the SVG spec is very large and documentation tends to be somewhat scattered. I hope to write an intro to it soon. In the meantime there is the #svg channel in the Elm slack for questions. For now, I would also recommend the [elm-community/typed-svg](https://elm.dmy.fr/packages/elm-community/typed-svg/latest/) over the [elm/svg](https://elm.dmy.fr/packages/elm/svg/latest/) package.
- Building data visualization requires an understanding of data concepts. Learn about basic statistical concepts, and get some experience with data cleaning and data preparation tasks.
- Finally, you should have a reasonable understanding of front-end programming in Elm. Start with the [guide](https://guide.elm-lang.org) if you don't.

You don't need to be an expert in these topics, but we'll assume basic familiarity with these things in this guide as well as in other elm-visualization material such as the API docs and the examples.

## Learning by Example

One of the main learning resources we provide is the [examples gallery](https://elm-visualization.netlify.app/). I encourage you to browse these. You can bootstrap your project by copy-pasting the example that looks the most like the thing that you are trying to build, then tweak it to do what you want. You can learn a lot by breaking and improving these examples. Once you get a little more experienced, reading the example code can reveal new and interesting techniques that you can apply to your own code.

On a technical note, these examples depend on a [variety of community packages](https://github.com/gampleman/elm-visualization/blob/master/examples/elm.json), so if you see compiler errors (especially about missing modules) when copying these examples, you might need to install some of these dependencies. Alternatively, you can start with [this Ellie](https://ellie-app.com/8592jsvBL2ka1) that should have the dependencies already added.

## Data

One of the first tasks you'll need to do is acquire data into your Elm program. Most of the examples simply hardcode the data as program literals, and indeed this can be a simple way to prototype. However, for many usecases you will likely want to load data from external sources. First you need to understand how you want to model and represent the data in your program, then you will need to write appropriate decoders from the wire format this data comes in. JSON is probably the easiest to work with (and the default elm/json package works very well for this), but CSV is also common (ericgj/elm-csv-decode and lovasoa/elm-csv are packages that can help here). If you are not hosting the data on your own server, than [CORS](https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS) might be something you need to look into.

A related task to decoding is cleaning: you need to decide on what to do with missing data, null data, invalid data and data that is out of bounds.

Once you have the cleaned and decoded data, elm-visualization provides the [`Statistics`](https://elm.dmy.fr/packages/gampleman/elm-visualization/latest/Statistics) module to help you understand the data in question:

```elm
data : List Float
data =
    [ 2, 4, 1, 4, 9 ]


Statistics.extent data --> Just ( 1, 9 )
```

## Scales

The most fundamental tool in all of elm-visualization, that you will use in nearly any data graphic, is the Scale. The purpose of scales is to transform an abstract dimension of data into a visual variables on the screen.

Imagine for example that we have the following dataset:

```elm
people : List { nationality : String, count: Int }
people =
    [ { nationality = "🇨🇳", count = 123 }
    , { nationality = "🇺🇸", count = 95 }
    , { nationality = "🇬🇧", count = 45 }
    , { nationality = "🇨🇿", count = 21 }
    , { nationality = "🇨🇾", count = 4 }
    ]
```

Here we have two dimensions:

- `count`, which we call _quantitative_:

      ```elm
      List.map .count people ---> [ 123, 95, 45, 21, 4 ]
      ```

- and `nationality`, which we call _nominal_ or _categorical_:

  ```elm
  List.map .nationality people ---> [ "🇨🇳", "🇺🇸", "🇬🇧", "🇨🇿", "🇨🇾" ]
  ```

There are many visual variables we can have on the screen. We can encode quantity, difference, similarity, grouping, hierarchy using size, color, position, animation, etc.

In the following bar chart, we encode two abstract dimensions to two visual variables: the _nationality_ dimension is encoded as the bars vertical position, whereas the _count_ is encoded as the horizontal length of the bars.

<!--
module BarChartExample exposing (main)

import Axis
import DateFormat
import Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import Time
import TypedSvg exposing (g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Transform(..))


w : Float
w =
    900


h : Float
h =
    250


padding : Float
padding =
    30


people : List { nationality : String, count : Int }
people =
    [ { nationality = "🇨🇳", count = 123 }
    , { nationality = "🇺🇸", count = 95 }
    , { nationality = "🇬🇧", count = 45 }
    , { nationality = "🇨🇿", count = 21 }
    , { nationality = "🇨🇾", count = 4 }
    ]


yScale : BandScale String
yScale =
    List.map .nationality people
        |> Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( 0, h - 2 * padding )

maxCount : Float
maxCount =
    people
        |> List.map .count
        |> List.maximum
        |> Maybe.withDefault 0
        |> toFloat


xScale : ContinuousScale Float
xScale =
    Scale.linear ( 0, w - 2 * padding) ( 0, maxCount )



yAxis : Svg msg
yAxis  =
    Axis.left [] (Scale.toRenderable identity yScale )


xAxis : Svg msg
xAxis =
    Axis.top [ Axis.tickCount 5 ] xScale


bar :  {nationality : String, count : Int } -> Svg msg
bar  {nationality, count } =
    g [ class [ "bar" ] ]
        [ rect
            [ x 0
            , y <| Scale.convert yScale nationality
            , height <| Scale.bandwidth yScale
            , width <| Scale.convert xScale (toFloat count)
            ]
            []
       , text_
            [ x <| Scale.convert xScale (toFloat count) - 5

            , y <| Scale.convert (Scale.toRenderable identity yScale) nationality + 5
            , textAnchor AnchorEnd
            ]
            [ text <| String.fromInt count ]
        ]


view : List { nationality : String, count : Int }-> Svg msg
view model =
    svg [ viewBox 0 0 w h ]
        [ style [] [ text """
            .bar rect { fill: rgba(118, 214, 78, 0.8); }

          """ ]
        , g [ transform [ Translate (padding - 1) ( padding) ] ]
            [ xAxis ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
            List.map bar model
        ]


main =
    view people
-->

<svg viewBox="0 0 900 250">
    <g transform="translate(29 30)"><g fill="none" font-size="10" font-family="sans-serif" text-anchor="middle"><path class="domain" stroke="#000" d="M0.5,-6V0.5H840.5V-6"></path><g class="tick" transform="translate(0, 0)"><line stroke="#000" y2="-6" x1="0.5" x2="0.5"></line><text fill="#000" y="-9" x="0.5" dy="0em">0</text></g><g class="tick" transform="translate(136.58536585365854, 0)"><line stroke="#000" y2="-6" x1="0.5" x2="0.5"></line><text fill="#000" y="-9" x="0.5" dy="0em">20</text></g><g class="tick" transform="translate(273.1707317073171, 0)"><line stroke="#000" y2="-6" x1="0.5" x2="0.5"></line><text fill="#000" y="-9" x="0.5" dy="0em">40</text></g><g class="tick" transform="translate(409.7560975609756, 0)"><line stroke="#000" y2="-6" x1="0.5" x2="0.5"></line><text fill="#000" y="-9" x="0.5" dy="0em">60</text></g><g class="tick" transform="translate(546.3414634146342, 0)"><line stroke="#000" y2="-6" x1="0.5" x2="0.5"></line><text fill="#000" y="-9" x="0.5" dy="0em">80</text></g><g class="tick" transform="translate(682.9268292682926, 0)"><line stroke="#000" y2="-6" x1="0.5" x2="0.5"></line><text fill="#000" y="-9" x="0.5" dy="0em">100</text></g><g class="tick" transform="translate(819.5121951219512, 0)"><line stroke="#000" y2="-6" x1="0.5" x2="0.5"></line><text fill="#000" y="-9" x="0.5" dy="0em">120</text></g></g></g><g transform="translate(29 30)"><g fill="none" font-size="10" font-family="sans-serif" text-anchor="end"><path class="domain" stroke="#000" d="M-6,0.5H0.5V190.5H-6"></path><g class="tick" transform="translate(0, 22.80188679245283)"><line stroke="#000" x2="-6" y1="0.5" y2="0.5"></line><text fill="#000" x="-9" y="0.5" dy="0.32em">🇨🇳</text></g><g class="tick" transform="translate(0, 58.65094339622641)"><line stroke="#000" x2="-6" y1="0.5" y2="0.5"></line><text fill="#000" x="-9" y="0.5" dy="0.32em">🇺🇸</text></g><g class="tick" transform="translate(0, 94.5)"><line stroke="#000" x2="-6" y1="0.5" y2="0.5"></line><text fill="#000" x="-9" y="0.5" dy="0.32em">🇬🇧</text></g><g class="tick" transform="translate(0, 130.34905660377356)"><line stroke="#000" x2="-6" y1="0.5" y2="0.5"></line><text fill="#000" x="-9" y="0.5" dy="0.32em">🇨🇿</text></g><g class="tick" transform="translate(0, 166.19811320754715)"><line stroke="#000" x2="-6" y1="0.5" y2="0.5"></line><text fill="#000" x="-9" y="0.5" dy="0.32em">🇨🇾</text></g></g></g><g transform="translate(30 30)" class="series"><g class="bar"><rect x="0px" y="7.169811320754718px" height="32.264150943396224px" width="840px"></rect><text x="835px" y="27.80188679245283px" text-anchor="end">123</text></g><g class="bar"><rect x="0px" y="43.0188679245283px" height="32.264150943396224px" width="648.780487804878px"></rect><text x="643.780487804878px" y="63.65094339622641px" text-anchor="end">95</text></g><g class="bar"><rect x="0px" y="78.86792452830188px" height="32.264150943396224px" width="307.3170731707317px"></rect><text x="302.3170731707317px" y="99.5px" text-anchor="end">45</text></g><g class="bar"><rect x="0px" y="114.71698113207546px" height="32.264150943396224px" width="143.41463414634148px"></rect><text x="138.41463414634148px" y="135.34905660377356px" text-anchor="end">21</text></g><g class="bar"><rect x="0px" y="150.56603773584905px" height="32.264150943396224px" width="27.31707317073171px"></rect><text x="22.31707317073171px" y="171.19811320754715px" text-anchor="end">4</text></g></g>
</svg>

```elm
import Scale exposing (ContinuousScale, BandScale)


maxCount : Float
maxCount =
    people
        |> List.map .count
        |> List.maximum
        |> Maybe.withDefault 0
        |> toFloat


xScale : ContinuousScale Float
xScale =
    Scale.linear ( padding, w - padding) ( 0, maxCount )


yScale : BandScale String
yScale =
    List.map .nationality people
        |> Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( padding, h - padding )
```

[Scales](https://package.elm-lang.org/packages/gampleman/elm-visualization/latest/Scale) come in many shapes and sizes depending on what kind of abstract dimensions you have and what kind of visual variables you want to encode your marks into. Most scales take 2 (or more) arguments: the visual variables you want to encode (the _range_) and the abstract dimension (the _domain_). For example the `xScale` is configured such that the count of 0 corresponds to the coordinate the charts left edge, and the `maxCount` corresponds to the charts right edge.

In the band scale the domain is a list of values (`[ "🇨🇳", "🇺🇸", "🇬🇧", "🇨🇿", "🇨🇾" ]`), while the range is a continous interval. The band scale figures out how to slice that interval into discrete padded bands.

The bars can than be easily rendered as `<rect>` elements using the values that our scales produce for us:

```elm
green : Color
green =
    Color.rgba 0.4609375 0.8359375 0.3046875 0.8

bar : { nationality : String, count : Int } -> Svg msg
bar { nationality, count } =
    g [ class [ "bar" ] ]
        [ rect
            [ x 0
            , y <| Scale.convert yScale nationality
            , height <| Scale.bandwidth yScale
            , width <| Scale.convert xScale (toFloat count)
            , fill <| Paint green
            ]
            []
        , text_
            [ x <| Scale.convert xScale (toFloat count)
            , y <| Scale.convert (Scale.toRenderable identity yScale) nationality
            , textAnchor AnchorEnd
            , dx -5
            , dy 6
            ]
            [ text <| String.fromInt count ]
        ]
```

<svg viewBox="0 0 900 30">
  <g class="bar">
    <rect x="30px" y="0" height="32.264150943396224px" width="648.780487804878px"></rect>
    <text x="643.780487804878px" y="30px" text-anchor="end" dx="-5" dy="21">95</text>
  </g>
</svg>

Now we can simply map over the the data applying this function:

```elm
view : List { nationality : String, count : Int } -> Svg msg
view model =
    svg [ viewBox 0 0 w h ]
        [ g [ class [ "series" ] ] <|
            List.map bar model
        ]
```

<svg viewBox="0 0 900 250">
   <g transform="translate(30 30)" class="series"><g class="bar"><rect x="0px" y="7.169811320754718px" height="32.264150943396224px" width="840px"></rect><text x="835px" y="27.80188679245283px" text-anchor="end">123</text></g><g class="bar"><rect x="0px" y="43.0188679245283px" height="32.264150943396224px" width="648.780487804878px"></rect><text x="643.780487804878px" y="63.65094339622641px" text-anchor="end">95</text></g><g class="bar"><rect x="0px" y="78.86792452830188px" height="32.264150943396224px" width="307.3170731707317px"></rect><text x="302.3170731707317px" y="99.5px" text-anchor="end">45</text></g><g class="bar"><rect x="0px" y="114.71698113207546px" height="32.264150943396224px" width="143.41463414634148px"></rect><text x="138.41463414634148px" y="135.34905660377356px" text-anchor="end">21</text></g><g class="bar"><rect x="0px" y="150.56603773584905px" height="32.264150943396224px" width="27.31707317073171px"></rect><text x="22.31707317073171px" y="171.19811320754715px" text-anchor="end">4</text></g></g>
</svg>

Another useful thing that scales provide is the [Axis](https://package.elm-lang.org/packages/gampleman/elm-visualization/latest/Axis) module,
which provides built in axes, which show the encoding explicetely.

```elm
xAxis : Svg msg
xAxis =
    g [ transform [ Translate 0 padding ] ]
        [ Axis.top [] xScale ]



yAxis : Svg msg
yAxis  =
    g [ transform [ Translate padding 0 ] ]
        [ Axis.left [] (Scale.toRenderable identity yScale) ]
```

(the `Scale.toRenderable` is needed specifically for Band scales so that labels on Axes are nicely centered on the bands)

<svg viewBox="0 0 900 250">
    <g transform="translate(29 30)"><g fill="none" font-size="10" font-family="sans-serif" text-anchor="middle"><path class="domain" stroke="#000" d="M0.5,-6V0.5H840.5V-6"></path><g class="tick" transform="translate(0, 0)"><line stroke="#000" y2="-6" x1="0.5" x2="0.5"></line><text fill="#000" y="-9" x="0.5" dy="0em">0</text></g><g class="tick" transform="translate(136.58536585365854, 0)"><line stroke="#000" y2="-6" x1="0.5" x2="0.5"></line><text fill="#000" y="-9" x="0.5" dy="0em">20</text></g><g class="tick" transform="translate(273.1707317073171, 0)"><line stroke="#000" y2="-6" x1="0.5" x2="0.5"></line><text fill="#000" y="-9" x="0.5" dy="0em">40</text></g><g class="tick" transform="translate(409.7560975609756, 0)"><line stroke="#000" y2="-6" x1="0.5" x2="0.5"></line><text fill="#000" y="-9" x="0.5" dy="0em">60</text></g><g class="tick" transform="translate(546.3414634146342, 0)"><line stroke="#000" y2="-6" x1="0.5" x2="0.5"></line><text fill="#000" y="-9" x="0.5" dy="0em">80</text></g><g class="tick" transform="translate(682.9268292682926, 0)"><line stroke="#000" y2="-6" x1="0.5" x2="0.5"></line><text fill="#000" y="-9" x="0.5" dy="0em">100</text></g><g class="tick" transform="translate(819.5121951219512, 0)"><line stroke="#000" y2="-6" x1="0.5" x2="0.5"></line><text fill="#000" y="-9" x="0.5" dy="0em">120</text></g></g></g><g transform="translate(29 30)"><g fill="none" font-size="10" font-family="sans-serif" text-anchor="end"><path class="domain" stroke="#000" d="M-6,0.5H0.5V190.5H-6"></path><g class="tick" transform="translate(0, 22.80188679245283)"><line stroke="#000" x2="-6" y1="0.5" y2="0.5"></line><text fill="#000" x="-9" y="0.5" dy="0.32em">🇨🇳</text></g><g class="tick" transform="translate(0, 58.65094339622641)"><line stroke="#000" x2="-6" y1="0.5" y2="0.5"></line><text fill="#000" x="-9" y="0.5" dy="0.32em">🇺🇸</text></g><g class="tick" transform="translate(0, 94.5)"><line stroke="#000" x2="-6" y1="0.5" y2="0.5"></line><text fill="#000" x="-9" y="0.5" dy="0.32em">🇬🇧</text></g><g class="tick" transform="translate(0, 130.34905660377356)"><line stroke="#000" x2="-6" y1="0.5" y2="0.5"></line><text fill="#000" x="-9" y="0.5" dy="0.32em">🇨🇿</text></g><g class="tick" transform="translate(0, 166.19811320754715)"><line stroke="#000" x2="-6" y1="0.5" y2="0.5"></line><text fill="#000" x="-9" y="0.5" dy="0.32em">🇨🇾</text></g></g></g><g transform="translate(30 30)" class="series"><g class="bar"><rect x="0px" y="7.169811320754718px" height="32.264150943396224px" width="840px"></rect><text x="835px" y="27.80188679245283px" text-anchor="end">123</text></g><g class="bar"><rect x="0px" y="43.0188679245283px" height="32.264150943396224px" width="648.780487804878px"></rect><text x="643.780487804878px" y="63.65094339622641px" text-anchor="end">95</text></g><g class="bar"><rect x="0px" y="78.86792452830188px" height="32.264150943396224px" width="307.3170731707317px"></rect><text x="302.3170731707317px" y="99.5px" text-anchor="end">45</text></g><g class="bar"><rect x="0px" y="114.71698113207546px" height="32.264150943396224px" width="143.41463414634148px"></rect><text x="138.41463414634148px" y="135.34905660377356px" text-anchor="end">21</text></g><g class="bar"><rect x="0px" y="150.56603773584905px" height="32.264150943396224px" width="27.31707317073171px"></rect><text x="22.31707317073171px" y="171.19811320754715px" text-anchor="end">4</text></g></g>
</svg>

So far our scales have been dealing only with position and size. But there's a lot more that scales can do:

```elm
colorScale : SequentialScale Color
colorScale =
    Scale.sequential Scale.Color.greensInterpolator ( 0, maxCount )
```

This uses a sequential scale which is just like a linear scale, except for the range it relies on an interpolator.
An interpolator is a function that takes a float between 0 and 1 and returns some value. In this case we are using
a built in interpolator from the `Scale.Color` module that ships with _many_ color schemes suitable for dataviz.
(Note that this uses the `Color` type from [avh4/elm-color](https://package.elm-lang.org/packages/avh4/elm-color/latest/)).

Let's modify our bar definition to include these colors:

```elm

bar : { nationality : String, count : Int } -> Svg msg
bar { nationality, count } =
    g [ class [ "bar" ] ]
        [ rect
            [ x 0
            , y <| Scale.convert yScale nationality
            , height <| Scale.bandwidth yScale
            , width <| Scale.convert xScale (toFloat count)
            , fill <| Paint <| Scale.convert colorScale <| toFloat count
            ]
            []
        , text_
            [ x <| Scale.convert xScale (toFloat count) - 5
            , y <| Scale.convert (Scale.toRenderable identity yScale) nationality + 5
            , textAnchor AnchorEnd
            , fill <| Paint <| if count > 60 then Color.white else Color.black
            ]
            [ text <| String.fromInt count ]
        ]
```
