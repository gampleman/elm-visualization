module StackedBarChart exposing (main)

import Axis
import Color exposing (Color)
import List.Extra as List
import SampleData exposing (CrimeRate)
import Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import Scale.Color
import Shape exposing (StackConfig, StackResult)
import TypedSvg exposing (g, rect, svg)
import TypedSvg.Attributes exposing (class, fill, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Transform(..))


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


reverseViridis : Float -> Color
reverseViridis progression =
    -- stylistic choice: the larger boxes look better in brighter colors, so invert the interpolator
    Scale.Color.viridisInterpolator (1 - progression)


colors : Int -> List Color
colors size =
    let
        colorScale =
            Scale.sequential reverseViridis ( 0, toFloat size - 1 )
                |> Scale.convert
    in
    List.range 0 (size - 1)
        |> List.map (colorScale << toFloat)


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
            Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( 0, w - (padding.top + padding.bottom) ) years

        yScale : ContinuousScale Float
        yScale =
            Scale.linear ( h - (padding.left + padding.right), 0 ) extent
                |> Scale.nice 4

        scaledValues =
            List.map (List.map (\( y1, y2 ) -> ( Scale.convert yScale y1, Scale.convert yScale y2 ))) yearValues
    in
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding.left - 1) (h - padding.bottom) ] ]
            [ Axis.bottom [ Axis.tickCount 10 ] (Scale.toRenderable String.fromInt xScale) ]
        , g [ transform [ Translate (padding.left - 1) padding.top ] ]
            [ Axis.left [] yScale ]
        , g [ transform [ Translate padding.left padding.top ], class [ "series" ] ] <|
            List.map (column xScale) (List.map2 (\a b -> ( a, b )) years scaledValues)
        ]
