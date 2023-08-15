module BackgroundGraph exposing (main)

{-| Part of a composition used for the background of my Elm Europe talk.

@category Art

-}

import Color exposing (Color)
import Force
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import IntDict
import List exposing (range)
import Scale exposing (SequentialScale)
import Scale.Color
import TypedSvg exposing (circle, g, line, polygon, svg, title)
import TypedSvg.Attributes exposing (class, fill, points, stroke, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth, x1, x2, y1, y2)
import TypedSvg.Core exposing (Attribute, Svg, text)
import TypedSvg.Types exposing (Paint(..))


w : Float
w =
    990


h : Float
h =
    504


colorScale : SequentialScale Color
colorScale =
    Scale.sequential Scale.Color.viridisInterpolator ( 200, 700 )


type alias CustomNode =
    { rank : Int, name : String }


type alias Entity =
    Force.Entity NodeId { value : CustomNode }


init : Graph Entity ()
init =
    let
        graph =
            Graph.mapContexts
                (\{ node, incoming, outgoing } ->
                    { incoming = incoming
                    , outgoing = outgoing
                    , node =
                        { label =
                            Force.entity node.id
                                (CustomNode
                                    (IntDict.size incoming + IntDict.size outgoing)
                                    node.label
                                )
                        , id = node.id
                        }
                    }
                )
                miserablesGraph

        links =
            graph
                |> Graph.edges
                |> List.map
                    (\{ from, to } ->
                        { source = from
                        , target = to
                        , distance = 30
                        , strength = Nothing
                        }
                    )

        forces =
            [ Force.customLinks 1 links
            , Force.manyBodyStrength -30 <| List.map .id <| Graph.nodes graph
            , Force.center (w / 2) (h / 2)
            ]
    in
    Graph.nodes graph
        |> List.map .label
        |> Force.computeSimulation (Force.simulation forces)
        |> updateGraphWithList graph


updateGraphWithList : Graph Entity () -> List Entity -> Graph Entity ()
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


updateContextWithValue : NodeContext Entity () -> Entity -> NodeContext Entity ()
updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
    { nodeCtx | node = { node | label = value } }


linkElement : Graph Entity () -> Edge () -> Svg msg
linkElement graph edge =
    let
        retrieveEntity =
            Maybe.withDefault (Force.entity 0 (CustomNode 0 "")) << Maybe.map (.node >> .label)

        source =
            retrieveEntity <| Graph.get edge.from graph

        target =
            retrieveEntity <| Graph.get edge.to graph
    in
    line
        [ strokeWidth 1
        , stroke <| Paint <| Scale.convert colorScale source.x
        , x1 source.x
        , y1 source.y
        , x2 target.x
        , y2 target.y
        ]
        []


hexagon : ( Float, Float ) -> Float -> List (Attribute msg) -> (List (Svg msg) -> Svg msg)
hexagon ( x, y ) size attrs =
    let
        angle =
            2 * pi / 6

        p =
            range 0 6
                |> List.map toFloat
                |> List.map (\a -> ( x + cos (a * angle) * size, y + sin (a * angle) * size ))
                |> points
    in
    polygon
        (p :: attrs)


nodeSize : Float -> Entity -> Svg msg
nodeSize size node =
    hexagon ( node.x, node.y )
        size
        [ fill <| Paint <| Scale.convert colorScale node.x
        ]
        [ title [] [ text node.value.name ] ]


nodeElement : Node Entity -> Svg msg
nodeElement node =
    if node.label.value.rank < 5 then
        nodeSize 4 node.label

    else if node.label.value.rank < 9 then
        nodeSize 7 node.label

    else if modBy 2 node.label.value.rank == 0 then
        g []
            [ nodeSize 9 node.label
            , circle
                [ r 12
                , cx node.label.x
                , cy node.label.y
                , fill PaintNone
                , stroke <| Paint <| Scale.convert colorScale node.label.x
                ]
                []
            ]

    else
        nodeSize 10 node.label


view : Graph Entity () -> Svg msg
view model =
    svg [ viewBox 0 0 w h ]
        [ g [ class [ "links" ] ] <| List.map (linkElement model) <| Graph.edges model
        , g [ class [ "nodes" ] ] <| List.map nodeElement <| Graph.nodes model
        ]


main : Svg msg
main =
    init |> view


miserablesGraph : Graph String ()
miserablesGraph =
    Graph.fromNodeLabelsAndEdgePairs
        [ "Myriel"
        , "Napoleon"
        , "Mlle.Baptistine"
        , "Mme.Magloire"
        , "CountessdeLo"
        , "Geborand"
        , "Champtercier"
        , "Cravatte"
        , "Count"
        , "OldMan"
        , "Labarre"
        , "Valjean"
        , "Marguerite"
        , "Mme.deR"
        , "Isabeau"
        , "Gervais"
        , "Tholomyes"
        , "Listolier"
        , "Fameuil"
        , "Blacheville"
        , "Favourite"
        , "Dahlia"
        , "Zephine"
        , "Fantine"
        , "Mme.Thenardier"
        , "Thenardier"
        , "Cosette"
        , "Javert"
        , "Fauchelevent"
        , "Bamatabois"
        , "Perpetue"
        , "Simplice"
        , "Scaufflaire"
        , "Woman1"
        , "Judge"
        , "Champmathieu"
        , "Brevet"
        , "Chenildieu"
        , "Cochepaille"
        , "Pontmercy"
        , "Boulatruelle"
        , "Eponine"
        , "Anzelma"
        , "Woman2"
        , "MotherInnocent"
        , "Gribier"
        , "Jondrette"
        , "Mme.Burgon"
        , "Gavroche"
        , "Gillenormand"
        , "Magnon"
        , "Mlle.Gillenormand"
        , "Mme.Pontmercy"
        , "Mlle.Vaubois"
        , "Lt.Gillenormand"
        , "Marius"
        , "BaronessT"
        , "Mabeuf"
        , "Enjolras"
        , "Combeferre"
        , "Prouvaire"
        , "Feuilly"
        , "Courfeyrac"
        , "Bahorel"
        , "Bossuet"
        , "Joly"
        , "Grantaire"
        , "MotherPlutarch"
        , "Gueulemer"
        , "Babet"
        , "Claquesous"
        , "Montparnasse"
        , "Toussaint"
        , "Child1"
        , "Child2"
        , "Brujon"
        , "Mme.Hucheloup"
        ]
        [ ( 1, 0 )
        , ( 2, 0 )
        , ( 3, 0 )
        , ( 3, 2 )
        , ( 4, 0 )
        , ( 5, 0 )
        , ( 6, 0 )
        , ( 7, 0 )
        , ( 8, 0 )
        , ( 9, 0 )
        , ( 11, 10 )
        , ( 11, 3 )
        , ( 11, 2 )
        , ( 11, 0 )
        , ( 12, 11 )
        , ( 13, 11 )
        , ( 14, 11 )
        , ( 15, 11 )
        , ( 17, 16 )
        , ( 18, 16 )
        , ( 18, 17 )
        , ( 19, 16 )
        , ( 19, 17 )
        , ( 19, 18 )
        , ( 20, 16 )
        , ( 20, 17 )
        , ( 20, 18 )
        , ( 20, 19 )
        , ( 21, 16 )
        , ( 21, 17 )
        , ( 21, 18 )
        , ( 21, 19 )
        , ( 21, 20 )
        , ( 22, 16 )
        , ( 22, 17 )
        , ( 22, 18 )
        , ( 22, 19 )
        , ( 22, 20 )
        , ( 22, 21 )
        , ( 23, 16 )
        , ( 23, 17 )
        , ( 23, 18 )
        , ( 23, 19 )
        , ( 23, 20 )
        , ( 23, 21 )
        , ( 23, 22 )
        , ( 23, 12 )
        , ( 23, 11 )
        , ( 24, 23 )
        , ( 24, 11 )
        , ( 25, 24 )
        , ( 25, 23 )
        , ( 25, 11 )
        , ( 26, 24 )
        , ( 26, 11 )
        , ( 26, 16 )
        , ( 26, 25 )
        , ( 27, 11 )
        , ( 27, 23 )
        , ( 27, 25 )
        , ( 27, 24 )
        , ( 27, 26 )
        , ( 28, 11 )
        , ( 28, 27 )
        , ( 29, 23 )
        , ( 29, 27 )
        , ( 29, 11 )
        , ( 30, 23 )
        , ( 31, 30 )
        , ( 31, 11 )
        , ( 31, 23 )
        , ( 31, 27 )
        , ( 32, 11 )
        , ( 33, 11 )
        , ( 33, 27 )
        , ( 34, 11 )
        , ( 34, 29 )
        , ( 35, 11 )
        , ( 35, 34 )
        , ( 35, 29 )
        , ( 36, 34 )
        , ( 36, 35 )
        , ( 36, 11 )
        , ( 36, 29 )
        , ( 37, 34 )
        , ( 37, 35 )
        , ( 37, 36 )
        , ( 37, 11 )
        , ( 37, 29 )
        , ( 38, 34 )
        , ( 38, 35 )
        , ( 38, 36 )
        , ( 38, 37 )
        , ( 38, 11 )
        , ( 38, 29 )
        , ( 39, 25 )
        , ( 40, 25 )
        , ( 41, 24 )
        , ( 41, 25 )
        , ( 42, 41 )
        , ( 42, 25 )
        , ( 42, 24 )
        , ( 43, 11 )
        , ( 43, 26 )
        , ( 43, 27 )
        , ( 44, 28 )
        , ( 44, 11 )
        , ( 45, 28 )
        , ( 47, 46 )
        , ( 48, 47 )
        , ( 48, 25 )
        , ( 48, 27 )
        , ( 48, 11 )
        , ( 49, 26 )
        , ( 49, 11 )
        , ( 50, 49 )
        , ( 50, 24 )
        , ( 51, 49 )
        , ( 51, 26 )
        , ( 51, 11 )
        , ( 52, 51 )
        , ( 52, 39 )
        , ( 53, 51 )
        , ( 54, 51 )
        , ( 54, 49 )
        , ( 54, 26 )
        , ( 55, 51 )
        , ( 55, 49 )
        , ( 55, 39 )
        , ( 55, 54 )
        , ( 55, 26 )
        , ( 55, 11 )
        , ( 55, 16 )
        , ( 55, 25 )
        , ( 55, 41 )
        , ( 55, 48 )
        , ( 56, 49 )
        , ( 56, 55 )
        , ( 57, 55 )
        , ( 57, 41 )
        , ( 57, 48 )
        , ( 58, 55 )
        , ( 58, 48 )
        , ( 58, 27 )
        , ( 58, 57 )
        , ( 58, 11 )
        , ( 59, 58 )
        , ( 59, 55 )
        , ( 59, 48 )
        , ( 59, 57 )
        , ( 60, 48 )
        , ( 60, 58 )
        , ( 60, 59 )
        , ( 61, 48 )
        , ( 61, 58 )
        , ( 61, 60 )
        , ( 61, 59 )
        , ( 61, 57 )
        , ( 61, 55 )
        , ( 62, 55 )
        , ( 62, 58 )
        , ( 62, 59 )
        , ( 62, 48 )
        , ( 62, 57 )
        , ( 62, 41 )
        , ( 62, 61 )
        , ( 62, 60 )
        , ( 63, 59 )
        , ( 63, 48 )
        , ( 63, 62 )
        , ( 63, 57 )
        , ( 63, 58 )
        , ( 63, 61 )
        , ( 63, 60 )
        , ( 63, 55 )
        , ( 64, 55 )
        , ( 64, 62 )
        , ( 64, 48 )
        , ( 64, 63 )
        , ( 64, 58 )
        , ( 64, 61 )
        , ( 64, 60 )
        , ( 64, 59 )
        , ( 64, 57 )
        , ( 64, 11 )
        , ( 65, 63 )
        , ( 65, 64 )
        , ( 65, 48 )
        , ( 65, 62 )
        , ( 65, 58 )
        , ( 65, 61 )
        , ( 65, 60 )
        , ( 65, 59 )
        , ( 65, 57 )
        , ( 65, 55 )
        , ( 66, 64 )
        , ( 66, 58 )
        , ( 66, 59 )
        , ( 66, 62 )
        , ( 66, 65 )
        , ( 66, 48 )
        , ( 66, 63 )
        , ( 66, 61 )
        , ( 66, 60 )
        , ( 67, 57 )
        , ( 68, 25 )
        , ( 68, 11 )
        , ( 68, 24 )
        , ( 68, 27 )
        , ( 68, 48 )
        , ( 68, 41 )
        , ( 69, 25 )
        , ( 69, 68 )
        , ( 69, 11 )
        , ( 69, 24 )
        , ( 69, 27 )
        , ( 69, 48 )
        , ( 69, 41 )
        , ( 70, 25 )
        , ( 70, 69 )
        , ( 70, 68 )
        , ( 70, 11 )
        , ( 70, 24 )
        , ( 70, 27 )
        , ( 70, 41 )
        , ( 70, 58 )
        , ( 71, 27 )
        , ( 71, 69 )
        , ( 71, 68 )
        , ( 71, 70 )
        , ( 71, 11 )
        , ( 71, 48 )
        , ( 71, 41 )
        , ( 71, 25 )
        , ( 72, 26 )
        , ( 72, 27 )
        , ( 72, 11 )
        , ( 73, 48 )
        , ( 74, 48 )
        , ( 74, 73 )
        , ( 75, 69 )
        , ( 75, 68 )
        , ( 75, 25 )
        , ( 75, 48 )
        , ( 75, 41 )
        , ( 75, 70 )
        , ( 75, 71 )
        , ( 76, 64 )
        , ( 76, 65 )
        , ( 76, 66 )
        , ( 76, 63 )
        , ( 76, 62 )
        , ( 76, 48 )
        , ( 76, 58 )
        ]
