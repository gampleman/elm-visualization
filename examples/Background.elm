module Background exposing (main)

{-| Part of a composition used for the background of my Elm Europe talk.
-}

import Color exposing (Color)
import Color.Convert exposing (colorToCssRgb)
import Graph exposing (Edge, Graph, Node, NodeId)
import IntDict
import List exposing (range)
import SampleData exposing (miserablesGraph)
import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)
import Visualization.Force as Force exposing (State)
import Visualization.Scale as Scale exposing (SequentialScale)


screenWidth : Float
screenWidth =
    990


screenHeight : Float
screenHeight =
    504


colorScale : SequentialScale Color
colorScale =
    Scale.sequential ( 200, 700 ) Scale.viridisInterpolator


type alias CustomNode =
    { rank : Int, name : String }


type alias Entity =
    Force.Entity NodeId { value : CustomNode }


init : Graph Entity ()
init =
    let
        graph =
            Graph.mapContexts
                (\({ node, incoming, outgoing } as ctx) ->
                    { ctx | node = { label = Force.entity node.id (CustomNode (IntDict.size incoming + IntDict.size outgoing) node.label), id = node.id } }
                )
                miserablesGraph

        links =
            graph
                |> Graph.edges
                |> List.map (\{ from, to } -> { source = from, target = to, distance = 30, strength = Nothing })

        forces =
            [ Force.customLinks 1 links
            , Force.manyBodyStrength -30 <| List.map .id <| Graph.nodes graph
            , Force.center (screenWidth / 2) (screenHeight / 2)
            ]
    in
        updateGraphWithList graph (Force.computeSimulation (Force.simulation forces) <| List.map .label <| Graph.nodes graph)


updateGraphWithList : Graph Entity () -> List Entity -> Graph Entity ()
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
        List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


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
            [ strokeWidth "1"
            , stroke <| colorToCssRgb <| Scale.convert colorScale source.x
            , x1 (toString source.x)
            , y1 (toString source.y)
            , x2 (toString target.x)
            , y2 (toString target.y)
            ]
            []


hexagon ( x, y ) size attrs =
    let
        angle =
            2 * pi / 6

        p =
            range 0 6
                |> List.map toFloat
                |> List.map (\a -> ( x + (cos (a * angle)) * size, y + (sin (a * angle)) * size ))
                |> List.map (\( x, y ) -> toString x ++ "," ++ toString y)
                |> String.join " "
                |> points
    in
        polygon
            (p :: attrs)


nodeSize size node =
    hexagon ( node.x, node.y )
        size
        [ fill <| colorToCssRgb <| Scale.convert colorScale node.x
        ]
        [ Svg.title [] [ text node.value.name ] ]


nodeElement node =
    if node.label.value.rank < 5 then
        nodeSize 4 node.label
    else if node.label.value.rank < 9 then
        nodeSize 7 node.label
    else if node.label.value.rank % 2 == 0 then
        g []
            [ nodeSize 9 node.label
            , circle
                [ r "12"
                , cx <| toString node.label.x
                , cy <| toString node.label.y
                , fill "none"
                , stroke <| colorToCssRgb <| Scale.convert colorScale node.label.x
                ]
                []
            ]
    else
        nodeSize 10 node.label


view model =
    svg [ width (toString screenWidth ++ "px"), height (toString screenHeight ++ "px") ]
        [ g [ class "links" ] <| List.map (linkElement model) <| Graph.edges model
        , g [ class "nodes" ] <| List.map nodeElement <| Graph.nodes model
        ]


main =
    init |> view
