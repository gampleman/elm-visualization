module StackedBarChart exposing (main)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Color.Convert exposing (colorToCssRgb)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Shape as Shape exposing (StackConfig, StackResult)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import SampleData exposing (CrimeRate)
import Color exposing (Color)
import List.Extra as List


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


canvas : { width : Float, height : Float }
canvas =
    { width = 990
    , height = 504
    }


padding : { bottom : number, left : number1, right : number2, top : number3 }
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


colors : Int -> List String
colors size =
    let
        -- given an index, give a value in [0,1] (0 for first, 1 for final)
        lengthScale =
            Scale.linear ( 0, toFloat size - 1 ) ( 0, 1 )
                |> Scale.convert
    in
        List.range 0 (size - 1)
            |> List.map (colorToCssRgb << sampleColor << lengthScale << toFloat)


column : BandScale Year -> ( Year, List ( Float, Float ) ) -> Svg msg
column xScale ( year, values ) =
    let
        block color ( upperY, lowerY ) =
            rect
                [ x <| toString <| Scale.convert xScale year
                , y <| toString <| lowerY
                , width <| toString <| Scale.bandwidth xScale
                , height <| toString <| (abs <| upperY - lowerY)
                , fill color
                ]
                []
    in
        g [ class "column" ] (List.map2 block (colors (List.length values)) values)


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
            Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } years ( 0, canvas.width - (padding.top + padding.bottom) )

        yScale : ContinuousScale
        yScale =
            Scale.linear extent ( canvas.height - (padding.left + padding.right), 0 )
                |> flip Scale.nice 4

        axisOptions =
            Axis.defaultOptions

        xAxis : Svg msg
        xAxis =
            Axis.axis { axisOptions | orientation = Axis.Bottom, tickCount = 10 } (Scale.toRenderable xScale)

        yAxis : Svg msg
        yAxis =
            Axis.axis { axisOptions | orientation = Axis.Left } yScale

        scaledValues =
            List.map (List.map (\( y1, y2 ) -> ( Scale.convert yScale y1, Scale.convert yScale y2 ))) yearValues
    in
        svg [ width (toString canvas.width ++ "px"), height (toString canvas.height ++ "px") ]
            [ g [ translate (padding.left - 1) (canvas.height - padding.bottom) ]
                [ xAxis ]
            , g [ translate (padding.left - 1) padding.top ]
                [ yAxis ]
            , g [ translate padding.left padding.top, class "series" ] <|
                List.map (column xScale) (List.map2 (,) years scaledValues)
            ]


translate : number -> number -> Svg.Attribute msg
translate x y =
    transform ("translate(" ++ toString x ++ ", " ++ toString y ++ ")")
