module Background exposing (main)

import AnimationFrame
import Color exposing (Color, toRgb)
import Dict exposing (Dict)
import Graph exposing (Edge, Graph, Node, NodeId)
import Html
import Html.Events exposing (on)
import IntDict
import Json.Decode as Decode
import List exposing (range)
import Mouse exposing (Position)
import NetworkGraphs exposing (pollbooksGraph)
import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)
import Time exposing (Time)
import Visualization.Force as Force exposing (Entity, State)
import Visualization.Scale as Scale exposing (SequentialScale)


screenWidth : Float
screenWidth =
    1440


screenHeight : Float
screenHeight =
    900


colorScale : SequentialScale Color
colorScale =
    Scale.sequential ( -100, 1000 ) Scale.viridisInterpolator


colorString : Color -> String
colorString color =
    let
        { red, green, blue } =
            toRgb color
    in
        "rgb(" ++ toString red ++ ", " ++ toString green ++ ", " ++ toString blue ++ ")"


type alias CustomNode =
    { rank : Int, name : String }


init =
    let
        graph =
            Graph.mapContexts
                (\({ node, incoming, outgoing } as ctx) ->
                    { ctx | node = { label = Force.entity node.id (CustomNode (IntDict.size incoming + IntDict.size outgoing) node.label.label), id = node.id } }
                )
                pollbooksGraph

        dict =
            graphToDict graph

        links =
            graph
                |> Graph.edges
                |> List.map (\{ from, to } -> { source = from, target = to, distance = 40, strength = Nothing })

        forces =
            [ Force.customLinks links
            , Force.manyBodyStrength -40 dict
            , Force.center (screenWidth / 2) (screenHeight / 2)
            ]
    in
        dictToGraph (Force.computeSimulation (Force.simulation forces) dict) graph


updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
        { nodeCtx | node = { node | label = value } }


graphToDict : Graph (Force.Entity a) () -> Dict NodeId (Force.Entity a)
graphToDict =
    Graph.fold (\{ node } d -> Dict.insert node.id node.label d) Dict.empty


dictToGraph : Dict NodeId (Force.Entity a) -> Graph (Force.Entity a) () -> Graph (Force.Entity a) ()
dictToGraph d g =
    let
        updateH nodeCtx =
            Dict.get nodeCtx.node.id d
                |> Maybe.withDefault (nodeCtx.node.label)
                |> updateContextWithValue nodeCtx
    in
        Graph.mapContexts updateH g


linkElement : Graph (Entity CustomNode) () -> Edge () -> Svg msg
linkElement graph edge =
    if edge.from % 2 == 0 then
        let
            source =
                Maybe.withDefault (Force.entity 0 (CustomNode 0 "")) <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

            target =
                Maybe.withDefault (Force.entity 0 (CustomNode 0 "")) <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
        in
            line
                [ strokeWidth "1"
                , stroke <| colorString <| Scale.convert colorScale source.x
                , x1 (toString source.x)
                , y1 (toString source.y)
                , x2 (toString target.x)
                , y2 (toString target.y)
                ]
                []
    else
        text ""


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
        [ fill <| colorString <| Scale.convert colorScale node.x
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
                , stroke <| colorString <| Scale.convert colorScale node.label.x
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
