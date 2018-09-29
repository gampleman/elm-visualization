module StackedBarChart exposing (main)

import Color exposing (Color)
import List.Extra as List
import SampleData exposing (CrimeRate)
import TypedSvg exposing (g, rect, svg)
import TypedSvg.Attributes exposing (class, fill, transform)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Transform(..))
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import Visualization.Shape as Shape exposing (StackConfig, StackResult)


main : Svg msg
main =
    view (Shape.stack config)


type alias Year =
    Int


series : List { label : String, accessor : CrimeRate -> Int }
series =
    [ { label = "Assault"
      , accessor = .assault
      }
    , { label = "Rape"
      , accessor = .rape
      }
    , { label = "Robbery"
      , accessor = .robbery
      }
    , { label = "Murder"
      , accessor = .murder
      }
    ]


samples : List ( String, List Float )
samples =
    List.map (\{ label, accessor } -> ( label, List.map (toFloat << accessor) SampleData.crimeRates )) series


w : Float
w =
    990


h : Float
h =
    504


padding : { bottom : Float, left : Float, right : Float, top : Float }
padding =
    { top = 30
    , left = 60
    , right = 30
    , bottom = 60
    }


config : StackConfig String
config =
    { data = samples
    , offset = Shape.stackOffsetNone
    , order =
        -- stylistic choice: largest (by sum of values) category at the bottom
        List.sortBy (Tuple.second >> List.sum >> negate)
    }


sampleColor : Float -> Color
sampleColor progression =
    -- stylistic choice: the larger boxes look better in brighter colors, so invert the interpolator
    Scale.viridisInterpolator (1 - progression)


colors : Int -> List Color
colors size =
    let
        -- given an index, give a value in [0,1] (0 for first, 1 for final)
        lengthScale =
            Scale.linear ( 0, toFloat size - 1 ) ( 0, 1 )
                |> Scale.convert
    in
    List.range 0 (size - 1)
        |> List.map (sampleColor << lengthScale << toFloat)


column : BandScale Year -> ( Year, List ( Float, Float ) ) -> Svg msg
column xScale ( year, values ) =
    let
        block color ( upperY, lowerY ) =
            rect
                [ x <| Scale.convert xScale year
                , y <| lowerY
                , width <| Scale.bandwidth xScale
                , height <| (abs <| upperY - lowerY)
                , fill (Fill color)
                ]
                []
    in
    g [ class [ "column" ] ] (List.map2 block (colors (List.length values)) values)


view : StackResult String -> Svg msg
view { values, labels, extent } =
    let
        -- transpose back to get the values per year
        yearValues =
            List.transpose values

        years =
            List.map .year SampleData.crimeRates

        xScale : BandScale Year
        xScale =
            Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } years ( 0, w - (padding.top + padding.bottom) )

        yScale : ContinuousScale
        yScale =
            Scale.linear extent ( h - (padding.left + padding.right), 0 )
                |> (\a -> Scale.nice a 4)

        axisOptions =
            Axis.defaultOptions

        xAxis : Svg msg
        xAxis =
            Axis.axis { axisOptions | orientation = Axis.Bottom, tickCount = 10 } (Scale.toRenderable String.fromInt xScale)

        yAxis : Svg msg
        yAxis =
            Axis.axis { axisOptions | orientation = Axis.Left } yScale

        scaledValues =
            List.map (List.map (\( y1, y2 ) -> ( Scale.convert yScale y1, Scale.convert yScale y2 ))) yearValues
    in
    svg [ width w, height h ]
        [ g [ transform [ Translate (padding.left - 1) (h - padding.bottom) ] ]
            [ xAxis ]
        , g [ transform [ Translate (padding.left - 1) padding.top ] ]
            [ yAxis ]
        , g [ transform [ Translate padding.left padding.top ], class [ "series" ] ] <|
            List.map (column xScale) (List.map2 (\a b -> ( a, b )) years scaledValues)
        ]
