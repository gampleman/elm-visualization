module Streamgraph exposing (..)

import SampleData exposing (CrimeRate)
import Color.Convert exposing (colorToCssRgb)
import Visualization.Shape as Shape
import Visualization.Scale as Scale exposing (ContinuousScale, OrdinalScale)
import Visualization.Axis as Axis exposing (Orientation(..))
import Path exposing (Path, moveTo, subpath, lineTo, closePath)
import List.Extra as List
import Visualization.List exposing (extent)
import Curve
import Color exposing (Color)
import Svg exposing (..)
import Svg.Attributes exposing (..)


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    60


series : List { label : String, accessor : CrimeRate -> Int }
series =
    [ { label = "Murder"
      , accessor = .murder
      }
    , { label = "Rape"
      , accessor = .rape
      }
    , { label = "Robbery"
      , accessor = .robbery
      }
    , { label = "Assault"
      , accessor = .assault
      }
    ]


accessors : List (CrimeRate -> Int)
accessors =
    List.map .accessor series


values : CrimeRate -> List Float
values i =
    List.map (\a -> toFloat <| a i) accessors


samples : List ( String, List Float )
samples =
    List.map (\{ label, accessor } -> ( label, List.map (toFloat << accessor) SampleData.crimeRates )) series
        |> Debug.log "samples"


colorScale : OrdinalScale String Color
colorScale =
    Scale.ordinal (List.map .label series) Scale.category20c


sampleColor : String -> Color
sampleColor =
    Scale.convert colorScale >> Maybe.withDefault Color.black


main : Svg msg
main =
    view samples


view : List ( String, List Float ) -> Svg a
view data =
    let
        ( labels, stacked ) =
            stack data

        labelPositions =
            List.map (List.last >> Maybe.withDefault ( 0, 0 ) >> (\( y1, y2 ) -> (y2 + y1) / 2)) stacked
                |> List.map (Scale.convert yScale)

        axisOptions =
            Axis.defaultOptions

        extent =
            SampleData.crimeRates
                |> List.map (.year >> toFloat)
                |> Visualization.List.extent
                |> Maybe.withDefault ( 1900, 1901 )

        internalXScale =
            Scale.linear ( 0, uncurry (flip (-)) extent ) ( 0, w - 2 * padding )

        xScale : ContinuousScale
        xScale =
            Scale.linear extent ( 0, w - 2 * padding )

        ( minimal, maximal ) =
            calculateExtremes stacked

        yScale : ContinuousScale
        yScale =
            Scale.linear ( minimal, maximal ) ( h - 2 * padding, 0 )

        --|> flip Scale.nice 4
        xAxis : Svg msg
        xAxis =
            Axis.axis { axisOptions | orientation = Axis.Bottom, tickCount = 10 } xScale

        yAxis : Svg msg
        yAxis =
            Axis.axis { axisOptions | orientation = Axis.Left {- , ticks = Just () -} } yScale

        size =
            List.length stacked

        pathElements =
            List.map2 (\( label, _ ) values -> renderStream ( internalXScale, yScale ) (Scale.convert colorScale label |> Maybe.withDefault Color.black) values) data stacked
    in
        -- Svg.svg [ Svg.height (toString h), Svg.width (toString w) ] (xAxis :: pathElements)
        Svg.svg [ width (toString w ++ "px"), height (toString h ++ "px") ]
            [ g [ translate (padding - 1) (h - padding) ]
                [ xAxis ]
            , g [ translate padding padding, class "series" ] pathElements
            , g [ translate (padding - 1) padding ]
                [ yAxis, text_ [ fontFamily "sans-serif", fontSize "10", x "5", y "5" ] [ text "Occurences" ] ]
            , g [ fontFamily "sans-serif", fontSize "10" ]
                (List.map2
                    (\label yPosition ->
                        g [ translate (w - padding + 10) (padding + yPosition) ]
                            [ text_ [ fill (sampleColor label |> colorToCssRgb) ] [ text label ] ]
                    )
                    labels
                    labelPositions
                )
            , g [ translate (w - padding) (padding + 20) ]
                [ text_ [ fontFamily "sans-serif", fontSize "20", textAnchor "end" ] [ text "Violent Crime in the US" ]
                , text_ [ fontFamily "sans-serif", fontSize "10", textAnchor "end", dy "1em" ] [ text "Source: fbi.gov" ]
                ]
            ]


translate : number -> number -> Svg.Attribute msg
translate x y =
    transform ("translate(" ++ toString x ++ ", " ++ toString y ++ ")")


stack : List ( a, List Float ) -> ( List a, List (List ( Float, Float )) )
stack items =
    let
        ( labels, values ) =
            items |> Shape.sortByInsideOut (Tuple.second >> List.sum) |> List.unzip
    in
        values
            |> List.map (List.indexedMap (\i e -> ( toFloat i, e )))
            |> Shape.stackOffsetWiggle
            |> (,) labels


{-| Calculates the minimal and maximal y values, for correct scaling
-}
calculateExtremes : List (List ( Float, Float )) -> ( Float, Float )
calculateExtremes coords =
    let
        folder ( y1, y2 ) ( accmin, accmax ) =
            ( Basics.min y1 y2 |> Basics.min accmin, Basics.max y1 y2 |> Basics.max accmax )
    in
        List.map (List.foldl folder ( 0, 0 )) coords
            |> List.foldl (\( mi, ma ) ( accmin, accmax ) -> ( Basics.min mi accmin, Basics.max ma accmax )) ( 0, 0 )


type alias Scales =
    ( Float, Float ) -> ( Float, Float )


{-| Calculate x and y scales - to make the paths fit the canvas width and height
-}
calculateScales : { width : Float, height : Float } -> List (List ( Float, Float )) -> ( ContinuousScale, ContinuousScale )
calculateScales { width, height } coords =
    let
        ( minimal, maximal ) =
            calculateExtremes coords

        -- the number of data points per label
        sizeX =
            List.head coords
                |> Maybe.map List.length
                |> Maybe.withDefault 0

        scaleX =
            Scale.linear ( 0, toFloat sizeX - 1 ) ( 0, width )

        scaleY =
            Scale.linear ( minimal, maximal ) ( height, 0 )
    in
        ( scaleX, scaleY )


{-| Renders one colored stream with given scaling

The result is a generator because we generate the  band color randomly
-}
renderStream : ( ContinuousScale, ContinuousScale ) -> Color -> List ( Float, Float ) -> Svg msg
renderStream scales color coords =
    let
        path =
            toArea (Curve.catmullRom 1.0 << repeatFirst) scales coords

        --toArea (Curve.linear) scales coords
        constructPath fillColor =
            Path.svgPath path [ fill fillColor ]
    in
        color
            |> colorToCssRgb
            |> constructPath


repeatFirst : List a -> List a
repeatFirst items =
    case items of
        [] ->
            []

        x :: xs ->
            x :: x :: xs


toArea : (List ( Float, Float ) -> Path) -> ( ContinuousScale, ContinuousScale ) -> List ( Float, Float ) -> Path
toArea curve ( scaleX, scaleY ) ys =
    let
        order ( y1, y2 ) =
            if y1 < y2 then
                ( y1, y2 )
            else
                ( y2, y1 )

        ( low_, high_ ) =
            List.map order ys
                |> List.unzip

        scale ( x, y ) =
            ( Scale.convert scaleX x
            , Scale.convert scaleY y
            )

        floatTuple i e =
            ( toFloat i, e )
                |> scale

        ( low, high ) =
            ( List.indexedMap floatTuple low_, List.indexedMap floatTuple high_ )
    in
        cycle curve low high


cycle : (List ( Float, Float ) -> Path) -> List ( Float, Float ) -> List ( Float, Float ) -> Path
cycle curve low high_ =
    let
        high =
            List.reverse high_
    in
        case ( low, high ) of
            ( l :: ls, h :: hs ) ->
                let
                    lastLow =
                        List.last ls |> Maybe.withDefault l

                    connectorRight =
                        lineTo [ lastLow, h ]

                    connectorLeft =
                        closePath
                in
                    case ( curve low, curve high ) of
                        ( [ bottom ], [ top ] ) ->
                            subpath (moveTo l) (bottom.drawtos ++ [ connectorRight ] ++ top.drawtos ++ [ connectorLeft ])
                                |> List.singleton

                        _ ->
                            []

            _ ->
                []
