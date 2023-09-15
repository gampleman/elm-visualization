module NorwegianCarSales exposing (main)

{-| This example demonstates using different kinds of layouts for stacked graphs.

@screenshot stream-graph
@screenshot silhouette
@screenshot stacked-area
@category Advanced

-}

import Axis
import Color exposing (Color)
import Example
import Html exposing (text)
import Interpolation
import List.Extra as List
import Path exposing (Path)
import Scale exposing (ContinuousScale, OrdinalScale)
import Scale.Color
import Shape exposing (StackResult)
import Time exposing (Month(..))
import Time.Extra exposing (Parts)
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (class, fill, fontFamily, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (fontSize)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..))


stacks : List ( String, StackResult String )
stacks =
    [ ( "Stream Graph"
      , Shape.stack
            { data = samples
            , offset = Shape.stackOffsetWiggle
            , order = Shape.sortByInsideOut (Tuple.second >> List.sum)
            }
      )
    , ( "Silhouette"
      , Shape.stack
            { data = samples
            , offset = Shape.stackOffsetSilhouette
            , order = Shape.sortByInsideOut (Tuple.second >> List.sum)
            }
      )
    , ( "Stacked Area"
      , Shape.stack
            { data = samples
            , offset = Shape.stackOffsetNone
            , order = List.sortBy (Tuple.second >> List.sum >> negate)
            }
      )
    , ( "Separated"
      , Shape.stack
            { data = samples
            , offset = stackOffsetSeparated
            , order = List.sortBy (Tuple.second >> List.sum >> negate)
            }
      )
    ]


{-| This is intended as a demonstration of how to write a custom stack offset function.
However, this is probably not the best way to make a small multiples chart (although, it
makes animating switching layouts straigtforward).
-}
stackOffsetSeparated : List (List ( Float, Float )) -> List (List ( Float, Float ))
stackOffsetSeparated series =
    List.foldl
        (\s1 ( maxSoFar, accum ) ->
            ( maxSoFar + (s1 |> List.map Tuple.second |> List.maximum |> Maybe.withDefault 0)
            , List.map (\( _, hi ) -> ( maxSoFar, maxSoFar + hi )) s1 :: accum
            )
        )
        ( 0, [] )
        series
        |> Tuple.second
        |> List.reverse


colorScale : OrdinalScale String Color
colorScale =
    List.map Tuple.first samples
        |> List.reverse
        |> Scale.ordinal Scale.Color.category10


sampleColor : String -> Color
sampleColor label =
    Scale.convert colorScale label |> Maybe.withDefault Color.black


colors : List String -> List Color
colors labels =
    List.map sampleColor labels


w : Float
w =
    990


h : Float
h =
    504


padding : Float
padding =
    40


fromCalendarDate : Int -> Month -> Int -> Time.Posix
fromCalendarDate year month day =
    Time.Extra.partsToPosix Time.utc (Parts year month day 0 0 0 0)


view : StackResult String -> Svg msg
view { values, labels, extent } =
    let
        labelsWidth =
            50

        size : Int
        size =
            List.head values
                |> Maybe.map List.length
                |> Maybe.withDefault 0

        xScale : ContinuousScale Float
        xScale =
            -- map an index to screen space
            Scale.linear ( padding, w - padding - labelsWidth ) ( 0, toFloat size - 1 )

        yScale : ContinuousScale Float
        yScale =
            Scale.linear ( h - padding, padding ) extent

        xAxis : Svg msg
        xAxis =
            -- construct the time domain for display
            -- the data is per-month, so we have to pick a day
            -- to get the ticks to show up correctly, the upper bound needs to be Jan 2 (Jan 1 does not work).
            Scale.time Time.utc ( 0, w - padding * 2 - labelsWidth ) ( fromCalendarDate 2007 Jan 1, fromCalendarDate 2017 Jan 2 )
                |> Axis.bottom [ Axis.tickCount 1 ]

        paths =
            List.map2 (renderStream ( xScale, yScale )) (colors labels) values

        labelPositions =
            let
                position ys =
                    ys
                        |> List.last
                        |> Maybe.withDefault ( 0, 0 )
                        |> (\( y1, y2 ) -> (y2 + y1) / 2)
                        |> Scale.convert yScale
            in
            List.map position values

        labelElement : String -> Float -> Svg msg
        labelElement label yPosition =
            g [ transform [ Translate (w - padding - labelsWidth + 10) yPosition ] ]
                [ text_ [ fill <| Paint <| sampleColor label ] [ text label ] ]
    in
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis ]
        , g [ class [ "series" ] ] paths
        , g [ fontFamily [ "sans-serif" ], fontSize 10 ]
            (List.map2 labelElement labels labelPositions)
        ]


{-| Renders one colored stream with given scaling
-}
renderStream : ( ContinuousScale Float, ContinuousScale Float ) -> Color -> List ( Float, Float ) -> Svg msg
renderStream scales color coords =
    Path.element (toArea scales coords) [ fill (Paint color) ]


{-| Create a svg path string that draws the area between two lines
-}
toArea : ( ContinuousScale Float, ContinuousScale Float ) -> List ( Float, Float ) -> Path
toArea ( scaleX, scaleY ) ys =
    let
        mapper : Int -> ( Float, Float ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
        mapper index ( y1, y2 ) =
            let
                xCoord =
                    index
                        |> toFloat
                        |> Scale.convert scaleX

                ( low, high ) =
                    if y1 < y2 then
                        ( y1, y2 )

                    else
                        ( y2, y1 )
            in
            Just
                ( ( xCoord, Scale.convert scaleY low )
                , ( xCoord, Scale.convert scaleY high )
                )
    in
    List.indexedMap mapper ys
        |> Shape.area Shape.monotoneInXCurve


interpolator : StackResult String -> StackResult String -> Float -> StackResult String
interpolator before after t =
    let
        extentInterpolator a b =
            Interpolation.tuple Interpolation.float Interpolation.float a b t
    in
    { extent = extentInterpolator before.extent after.extent
    , labels = List.sort after.labels
    , values = map2WithOrders (List.map2 extentInterpolator) before.labels after.labels before.values after.values
    }


map2WithOrders : (a -> b -> c) -> List String -> List String -> List a -> List b -> List c
map2WithOrders fn aOrd bOrd aList bList =
    List.map2 (\( _, a ) ( _, b ) -> fn a b)
        (List.sortBy Tuple.first (List.map2 Tuple.pair aOrd aList))
        (List.sortBy Tuple.first (List.map2 Tuple.pair bOrd bList))


main : Example.Program (StackResult String)
main =
    stacks
        |> Example.tabbed "Layout: "
        |> Example.animatedWith interpolator 1000
        |> Example.withTitle "Car Sales in Norway"
        |> Example.withCustomCss """
            .form {
                position: absolute;
                padding: 40px;
            }
            """
        |> Example.application view


{-| Subset of a kaggle dataset

<https://www.kaggle.com/dmi3kno/newcarsalesnorway>

which in turn is based on (I assume scraped from)

<http://www.ofvas.no/bilsalget/category404.html>

-}
samples : List ( String, List Float )
samples =
    [ ( "Nissan", [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 333, 0, 424, 543, 402, 385, 428, 447, 268, 193, 209, 246, 204, 200, 130, 142, 182, 166, 162, 202, 325, 323, 302, 307, 291, 231, 279, 257, 381, 386, 381, 424, 312, 280, 367, 315, 306, 182, 303, 272, 282, 258, 349, 307, 305, 262, 430, 325, 271, 206, 541, 439, 472, 310, 439, 365, 449, 445, 481, 516, 478, 260, 542, 482, 556, 721, 591, 570, 557, 648, 532, 956, 643, 433, 650, 776, 828, 785, 700, 521, 572, 599, 556, 668, 502, 499, 740, 657, 798, 314, 364, 493, 624, 536, 738, 577, 297, 138, 571, 752, 867, 667, 452, 472, 420, 449, 345, 526, 456, 409 ] )
    , ( "Skoda", [ 481, 334, 295, 253, 279, 295, 290, 258, 192, 256, 258, 308, 216, 225, 415, 268, 268, 220, 167, 139, 152, 117, 117, 181, 92, 197, 236, 286, 232, 182, 195, 169, 194, 318, 220, 211, 197, 272, 278, 272, 178, 263, 282, 177, 258, 258, 323, 256, 257, 278, 209, 238, 241, 245, 283, 274, 331, 357, 257, 321, 0, 219, 210, 297, 220, 401, 356, 217, 179, 255, 190, 383, 0, 0, 243, 252, 187, 184, 348, 324, 444, 540, 524, 702, 378, 308, 301, 398, 412, 381, 336, 354, 323, 483, 361, 392, 289, 362, 466, 521, 329, 358, 343, 372, 311, 386, 310, 349, 462, 688, 473, 569, 561, 496, 449, 392, 482, 489, 496, 384 ] )
    , ( "Mitsubishi", [ 0, 0, 0, 0, 0, 0, 0, 524, 410, 236, 118, 226, 232, 257, 430, 474, 205, 180, 194, 135, 182, 125, 110, 149, 0, 0, 150, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 233, 198, 265, 236, 217, 199, 141, 122, 123, 283, 252, 202, 222, 336, 642, 577, 363, 477, 338, 107, 133, 382, 415, 238, 171, 186, 200, 240, 176, 182, 185, 159, 133, 312, 262, 392, 267, 234, 387, 396, 260, 228, 188, 165, 177, 145, 305, 174, 238, 341, 311, 248, 211, 217, 280, 346, 229, 230, 163, 326, 157, 204, 280, 399, 487, 126, 88, 222, 527, 578, 561, 145, 553, 739, 569, 553, 590, 552, 405, 455, 365, 406, 355 ] )
    , ( "Audi", [ 471, 356, 423, 390, 437, 345, 424, 379, 280, 374, 118, 139, 157, 200, 235, 504, 467, 555, 504, 491, 435, 457, 341, 388, 89, 138, 160, 148, 151, 186, 246, 224, 133, 217, 226, 257, 176, 143, 207, 494, 388, 343, 248, 237, 144, 178, 205, 161, 0, 0, 0, 175, 242, 203, 192, 193, 205, 184, 178, 185, 159, 0, 0, 0, 0, 143, 0, 185, 119, 160, 216, 140, 179, 169, 139, 247, 408, 159, 407, 348, 344, 372, 269, 332, 151, 0, 201, 240, 219, 196, 222, 200, 146, 221, 157, 205, 119, 207, 281, 327, 303, 292, 321, 198, 148, 192, 209, 179, 427, 416, 266, 275, 266, 241, 419, 164, 186, 203, 270, 403 ] )
    , ( "Peugeot", [ 801, 493, 600, 409, 556, 421, 570, 372, 291, 491, 327, 112, 337, 144, 130, 213, 379, 138, 405, 350, 243, 328, 260, 73, 209, 204, 278, 402, 256, 114, 138, 99, 62, 69, 85, 39, 718, 291, 308, 220, 199, 193, 201, 203, 160, 183, 165, 115, 167, 192, 177, 222, 149, 309, 365, 315, 353, 0, 312, 231, 186, 169, 133, 128, 198, 289, 0, 0, 0, 0, 195, 0, 374, 187, 148, 0, 331, 0, 0, 0, 143, 0, 0, 72, 343, 184, 0, 0, 0, 0, 0, 0, 0, 260, 0, 206, 0, 0, 168, 181, 196, 203, 174, 145, 112, 170, 136, 0, 236, 212, 142, 170, 152, 153, 114, 0, 0, 0, 0, 0 ] )
    , ( "BMW", [ 183, 149, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 158, 184, 0, 208, 174, 207, 140, 86, 219, 196, 124, 189, 0, 135, 132, 133, 168, 188, 169, 194, 198, 175, 130, 130, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 168, 0, 0, 0, 0, 0, 0, 0, 0, 145, 0, 0, 237, 132, 186, 199, 223, 357, 165, 139, 176, 170, 298, 221, 387, 0, 0, 0, 449, 0, 264, 0, 482, 114, 141, 328, 421, 329, 556, 452, 296, 269, 265, 341, 257, 130, 156, 0, 0, 0, 499, 453, 0, 0, 0, 0, 266, 326, 293, 415, 846, 676, 904, 910, 306, 267, 229, 293, 520, 503, 1264, 523 ] )
    , ( "Mazda", [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 189, 228, 195, 202, 153, 109, 87, 97, 127, 248, 103, 54, 123, 89, 90, 164, 68, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 204, 0, 0, 0, 0, 137, 226, 490, 497, 489, 243, 167, 336, 502, 506, 421, 299, 250, 187, 406, 214, 389, 229, 366, 283, 323, 360, 361, 331, 368, 216, 252, 180, 180, 355, 286, 358, 248, 225, 256, 281, 262, 285, 492, 500, 369, 398, 399, 465, 354, 395, 428, 422, 326, 393 ] )
    , ( "Tesla", [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 616, 0, 0, 553, 0, 431, 1493, 171, 371, 536, 114, 183, 104, 36, 202, 267, 0, 321, 1140, 230, 346, 566, 222, 220, 127, 198, 225, 373, 105, 113, 487, 96, 156, 284, 43, 170, 247, 39, 48, 263 ] )
    ]
