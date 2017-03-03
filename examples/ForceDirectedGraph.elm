module ForceDirectedGraph exposing (main)

import Graph exposing (Edge, Graph, Node, NodeId)
import Html
import Html.Events exposing (on)
import Json.Decode as Decode
import Mouse exposing (Position, position)
import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)


-- import Visualization.Force as Force

import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)
import Mouse exposing (Position)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- list of nodes
-- MODEL


type alias NodeValue =
    { x : Float
    , y : Float
    , label : String
    }


type alias Model =
    { drag : Maybe Drag
    , graph : Graph NodeValue ()
    }


type alias Drag =
    { start : Position
    , current : Position
    , index : NodeId
    }


init : ( Model, Cmd Msg )
init =
    let
        graph =
            Graph.fromNodeLabelsAndEdgePairs [ node "Node 1", node "Node 2", node "Node 3" ] [ ( 0, 1 ), ( 0, 2 ), ( 1, 2 ) ]
    in
        ( Model Nothing graph, Cmd.none )


node =
    NodeValue 0 0



-- UPDATE


type Msg
    = DragStart NodeId Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateHelp msg model, Cmd.none )


updateNode pos nodeCtx =
    let
        nodeValue =
            nodeCtx.node.label

        node =
            nodeCtx.node
    in
        { nodeCtx | node = { node | label = { nodeValue | x = toFloat pos.x, y = toFloat pos.y } } }


updateHelp : Msg -> Model -> Model
updateHelp msg ({ drag, graph } as model) =
    case msg of
        DragStart index xy ->
            Model (Just (Drag xy xy index)) graph

        DragAt xy ->
            case drag of
                Just { start, index } ->
                    Model (Just (Drag start xy index)) (Graph.update index (Maybe.map (updateNode xy)) graph)

                Nothing ->
                    Model Nothing graph

        DragEnd xy ->
            case drag of
                Just { start, index } ->
                    Model Nothing (Graph.update index (Maybe.map (updateNode xy)) graph)

                Nothing ->
                    Model Nothing graph



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]



-- VIEW


onMouseDown : NodeId -> Attribute Msg
onMouseDown index =
    on "mousedown" (Decode.map (DragStart index) Mouse.position)


linkElement graph edge =
    let
        source =
            Maybe.withDefault (node "") <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

        target =
            Maybe.withDefault (node "") <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
    in
        line
            [ strokeWidth "1"
              --(toString (sqrt link.value))
            , stroke "#000"
            , x1 (toString source.x)
            , y1 (toString source.y)
            , x2 (toString target.x)
            , y2 (toString target.y)
            ]
            []


nodeElement node =
    circle
        [ r "5"
        , fill "#000"
          -- , fill (computeColor node.group)
          -- , Attr.title node.id
        , onMouseDown node.id
        , cx (toString node.label.x)
        , cy (toString node.label.y)
        ]
        []


view : Model -> Svg Msg
view model =
    svg [ width "100%", height "100%" ]
        [ g [ class "links" ] <| List.map (linkElement model.graph) <| Graph.edges model.graph
        , g [ class "nodes" ] <| List.map nodeElement <| Graph.nodes model.graph
        ]


computeColor a =
    "#000"
