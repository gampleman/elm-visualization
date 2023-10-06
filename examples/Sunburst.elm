module Sunburst exposing (main)

{-| We can use Hierarchy to visualize data that is not naturally in a tree like format.
In this example the data is a list of sequences of page visits users have made on
a website and looks like this:

    account-account-account-account-account-account,22781
    account-account-account-account-account-end,3311
    account-account-account-account-account-home,906
    ...

We turn this into a tree-like data structure by making the parent of each item
its prefix (so the parent of `account-account-account-account-account-home` is
`account-account-account-account-account`), then aggregating the counts up the
tree.

To make this visualization performant and sensible, the sequences are limited
in the dataset to be six or less and the long tail of pages is aggregated into
an `other` category. We distinguish truncated and complete sequences by adding
the `end` token to a complete sequence (the `end` can be understood as the user
leaving the website).

For serious deployment, a server could provide additional data combined with
zooming in on subsets interactively.

Based on work by Kerry Roden (under Apache 2 License).

@requires data/visit-sequences.csv
@category Advanced
@delay 1

-}

import Browser
import Color exposing (Color)
import Csv.Decode as Csv
import Curve
import Example
import Hierarchy
import Html exposing (Html)
import Http
import List.Extra
import Path exposing (Path)
import Scale exposing (OrdinalScale)
import Scale.Color
import Set
import Shape
import Svg.Lazy
import Tree exposing (Tree)
import TypedSvg exposing (g, rect, svg, text_)
import TypedSvg.Attributes exposing (dy, fill, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, rx, strokeWidth, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Events
import TypedSvg.Types exposing (AnchorAlignment(..), Opacity(..), Paint(..), Transform(..), em)



-- Constants


w : Float
w =
    990


h : Float
h =
    504


radius : Float
radius =
    min w h / 2


spacing : Float
spacing =
    50


breadCrumbHeight : Float
breadCrumbHeight =
    40


breadCrumbWidth : Float
breadCrumbWidth =
    140


arrowProtrusion : Float
arrowProtrusion =
    10



-- Types


type alias Datum =
    { sequence : List String, category : String, visits : Int }


type alias Data =
    Tree Datum


type alias LayedOutDatum =
    { x : Float, y : Float, width : Float, height : Float, value : Float, node : Datum }


type alias LoadedModel =
    { layout : List LayedOutDatum
    , hovered : Maybe { sequence : List String, percentage : Float }
    , total : Float
    }


type Model
    = Loading
    | Error Http.Error
    | Loaded LoadedModel


type Msg
    = ReceivedData (Result Http.Error Data)
    | Hover (Maybe { sequence : List String, percentage : Float })



-- Data loading and processing


init : () -> ( Model, Cmd Msg )
init () =
    ( Loading
    , Http.get
        { url = "data/visit-sequences.csv"
        , expect = expectCsv ReceivedData
        }
    )


expectCsv : (Result Http.Error Data -> msg) -> Http.Expect msg
expectCsv tagger =
    Http.expectString
        (Result.andThen
            (Csv.decodeCsv Csv.NoFieldNames decoder
                >> Result.mapError (Csv.errorToString >> Http.BadBody)
            )
            >> Result.andThen
                (Tree.stratifyWithPath
                    { path = \item -> List.Extra.inits item.sequence
                    , createMissingNode = \path -> { sequence = List.Extra.last path |> Maybe.withDefault [], visits = 0 }
                    }
                    >> Result.mapError (always (Http.BadBody "Tree creation failed"))
                )
            >> Result.map
                (Tree.sumUp identity
                    (\node children ->
                        { node | visits = List.sum (List.map .visits children) }
                    )
                    >> Tree.map
                        (\d ->
                            { sequence = d.sequence
                            , visits = d.visits
                            , category = List.Extra.last d.sequence |> Maybe.withDefault "end"
                            }
                        )
                    >> Tree.sortWith (\_ a b -> compare (Tree.label b).visits (Tree.label a).visits)
                )
            >> tagger
        )


decoder : Csv.Decoder { sequence : List String, visits : Int }
decoder =
    Csv.into
        (\sequence count ->
            { sequence = sequence, visits = count }
        )
        |> Csv.pipeline (Csv.map (String.split "-") (Csv.column 0 Csv.string))
        |> Csv.pipeline (Csv.column 1 Csv.int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ReceivedData (Ok rawData), _ ) ->
            ( Loaded
                { layout =
                    rawData
                        |> Hierarchy.partition [ Hierarchy.size (2 * pi) (radius * radius) ] (.visits >> toFloat)
                        |> Tree.toList
                        |> List.tail
                        |> Maybe.withDefault []
                        |> List.filter (\d -> d.width > 0.001)
                , total = toFloat (Tree.label rawData).visits
                , hovered = Nothing
                }
            , Cmd.none
            )

        ( ReceivedData (Err e), _ ) ->
            ( Error e, Cmd.none )

        ( Hover hover, Loaded mod ) ->
            ( Loaded { mod | hovered = hover }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- Visualization


colorScale : OrdinalScale String Color
colorScale =
    Scale.ordinal (Color.rgb 0.5 0.5 0.5 :: Scale.Color.tableau10) [ "end", "home", "product", "search", "account", "other" ]


view : Model -> Html Msg
view model =
    case model of
        Loaded data ->
            svg [ viewBox 0 0 w h ]
                [ sunburst data
                , breadcrumbs data
                ]

        Loading ->
            Example.loading []

        Error e ->
            Example.error Nothing e


arrow : Path
arrow =
    [ Curve.linearClosed [ ( 0, 0 ), ( breadCrumbWidth, 0 ), ( breadCrumbWidth, breadCrumbHeight ), ( breadCrumbWidth / 2, breadCrumbHeight + arrowProtrusion ), ( 0, breadCrumbHeight ) ] ]


breadcrumbs : LoadedModel -> Svg msg
breadcrumbs model =
    case model.hovered of
        Just { sequence, percentage } ->
            sequence
                |> List.indexedMap
                    (\i el ->
                        g [ transform [ Translate (radius * 2 + spacing) (toFloat i * breadCrumbHeight) ] ]
                            [ if el == "end" then
                                rect
                                    [ width breadCrumbWidth
                                    , height (breadCrumbHeight + arrowProtrusion)
                                    , fill (Paint (Scale.convert colorScale el |> Maybe.withDefault Color.black))
                                    , rx 3
                                    , stroke (Paint Color.white)
                                    , strokeWidth 3
                                    ]
                                    []

                              else
                                Path.element arrow
                                    [ fill (Paint (Scale.convert colorScale el |> Maybe.withDefault Color.black))
                                    , stroke (Paint Color.white)
                                    , strokeWidth 3
                                    ]
                            , text_
                                [ x (breadCrumbWidth / 2)
                                , y breadCrumbHeight
                                , textAnchor AnchorMiddle
                                , fill (Paint Color.white)
                                , dy (em -0.3)
                                , TypedSvg.Attributes.InPx.fontSize 14
                                , TypedSvg.Attributes.fontFamily [ "sans-serif" ]
                                ]
                                [ text el ]
                            ]
                    )
                |> List.append
                    [ text_
                        [ x (radius * 2 + spacing + breadCrumbWidth / 2)
                        , y (toFloat (List.length sequence + 1) * breadCrumbHeight)
                        , textAnchor AnchorMiddle
                        , dy (em -0.6)
                        , TypedSvg.Attributes.InPx.fontSize 14
                        , TypedSvg.Attributes.fontFamily [ "sans-serif" ]
                        ]
                        [ text (format percentage) ]
                    ]
                |> List.reverse
                |> g []

        Nothing ->
            text ""


sunburst : LoadedModel -> Svg Msg
sunburst model =
    let
        hovered =
            case model.hovered of
                Just { sequence } ->
                    List.Extra.inits sequence
                        |> Set.fromList

                Nothing ->
                    Set.empty

        opacity seq =
            TypedSvg.Attributes.fillOpacity
                (Opacity
                    (if
                        case model.hovered of
                            Just _ ->
                                Set.member seq hovered

                            Nothing ->
                                True
                     then
                        0.8

                     else
                        0.3
                    )
                )
    in
    g [ transform [ Translate radius radius ] ]
        [ g []
            (model.layout
                |> List.map
                    (\item ->
                        Path.element (arc item)
                            [ opacity item.node.sequence
                            , fill (Paint (Scale.convert colorScale item.node.category |> Maybe.withDefault Color.black))
                            ]
                    )
            )
        , Svg.Lazy.lazy2 mouseInteractionArcs model.layout model.total
        , case model.hovered of
            Just { percentage, sequence } ->
                g [ textAnchor AnchorMiddle, TypedSvg.Attributes.fontFamily [ "sans-serif" ], fill (Paint (Color.rgb 0.5 0.5 0.5)) ]
                    [ text_ [ TypedSvg.Attributes.InPx.fontSize 28, y -8 ] [ text (format percentage) ]
                    , text_ [ TypedSvg.Attributes.InPx.fontSize 10, y 10 ]
                        [ text
                            (if List.Extra.last sequence == Just "end" then
                                "of visits complete this sequence"

                             else
                                "of visits begin with this sequence"
                            )
                        ]
                    ]

            Nothing ->
                text ""
        ]


mouseInteractionArcs : List LayedOutDatum -> Float -> Svg Msg
mouseInteractionArcs segments total =
    g [ TypedSvg.Attributes.pointerEvents "all", TypedSvg.Events.onMouseLeave (Hover Nothing) ]
        (segments
            |> List.map
                (\item ->
                    Path.element (mouseArc item)
                        [ fill PaintNone
                        , TypedSvg.Events.onMouseEnter
                            (Hover
                                (Just
                                    { sequence = item.node.sequence
                                    , percentage = 100 * item.value / total
                                    }
                                )
                            )
                        ]
                )
        )


arc : LayedOutDatum -> Path
arc s =
    Shape.arc
        { innerRadius = sqrt s.y
        , outerRadius = sqrt (s.y + s.height) - 1
        , cornerRadius = 0
        , startAngle = s.x
        , endAngle = s.x + s.width
        , padAngle = 1 / radius
        , padRadius = radius
        }


mouseArc : LayedOutDatum -> Path
mouseArc s =
    Shape.arc
        { innerRadius = sqrt s.y
        , outerRadius = radius
        , cornerRadius = 0
        , startAngle = s.x
        , endAngle = s.x + s.width
        , padAngle = 0
        , padRadius = 0
        }


format : Float -> String
format f =
    String.left 5 (String.fromFloat f) ++ "%"


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
