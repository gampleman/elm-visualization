module AggregateQuadTree exposing (..)

{-| A quadtree that can store an aggregate in the nodes.
Intended for use in n-body simulation, specifically Barnes-Hut

https://en.wikipedia.org/wiki/Barnes%E2%80%93Hut_simulation#Calculating_the_force_acting_on_a_body

-}

import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)


type AggregateQuadTree aggregate item
    = Empty
    | Leaf { boundingBox : BoundingBox2d, aggregate : aggregate, children : ( item, List item ) }
    | Node
        { boundingBox : BoundingBox2d
        , aggregate : aggregate
        , nw : AggregateQuadTree aggregate item
        , ne : AggregateQuadTree aggregate item
        , se : AggregateQuadTree aggregate item
        , sw : AggregateQuadTree aggregate item
        }


type alias Config aggregate vertex =
    { combineVertices : vertex -> List vertex -> aggregate
    , combineAggregates : aggregate -> List aggregate -> aggregate
    , toPoint : vertex -> Point2d
    }


{-| An empty aggregate tree
-}
empty : AggregateQuadTree aggregate item
empty =
    Empty


{-| A singleton tree, using () as the aggregate
-}
singleton : (vertex -> Point2d) -> vertex -> AggregateQuadTree () vertex
singleton toPoint vertex =
    Leaf
        { boundingBox = BoundingBox2d.singleton (toPoint vertex)
        , children = ( vertex, [] )
        , aggregate = ()
        }


size : AggregateQuadTree aggregate vertex -> Int
size qtree =
    case qtree of
        Empty ->
            0

        Leaf leaf ->
            let
                ( _, rest ) =
                    leaf.children
            in
                1 + List.length rest

        Node node ->
            size node.nw + size node.ne + size node.se + size node.sw


insertBy : (vertex -> Point2d) -> vertex -> AggregateQuadTree () vertex -> AggregateQuadTree () vertex
insertBy toPoint vertex qtree =
    case qtree of
        Empty ->
            Leaf
                { boundingBox = BoundingBox2d.singleton (toPoint vertex)
                , children = ( vertex, [] )
                , aggregate = ()
                }

        Leaf leaf ->
            let
                ( first, rest ) =
                    leaf.children

                newSize =
                    2 + List.length rest

                -- this seems to be a good choice.
                -- normally, all vertices are put into one large leaf. When it reaches this size
                -- the leaf is split. From then on the node will resize when it encounters a point outside of its bounding box.
                -- resizing is expensive, so we wait a while before splitting. On the other hand, if we never split at all,
                -- we get none of the benefits of using an quadtree.
                maxSize =
                    32
            in
                if newSize >= maxSize then
                    let
                        initial =
                            { ne = [], nw = [], se = [], sw = [] }

                        folder element accum =
                            case quadrant leaf.boundingBox (toPoint element) of
                                NE ->
                                    { accum | ne = element :: accum.ne }

                                SE ->
                                    { accum | se = element :: accum.se }

                                NW ->
                                    { accum | nw = element :: accum.nw }

                                SW ->
                                    { accum | sw = element :: accum.sw }

                        byQuadrant : { ne : List vertex, nw : List vertex, se : List vertex, sw : List vertex }
                        byQuadrant =
                            List.foldl folder initial (vertex :: first :: rest)
                    in
                        Node
                            { boundingBox = BoundingBox2d.hull leaf.boundingBox (BoundingBox2d.singleton (toPoint vertex))
                            , ne = fromList toPoint byQuadrant.ne
                            , se = fromList toPoint byQuadrant.se
                            , nw = fromList toPoint byQuadrant.nw
                            , sw = fromList toPoint byQuadrant.sw
                            , aggregate = ()
                            }
                else
                    Leaf
                        { boundingBox = BoundingBox2d.hull leaf.boundingBox (BoundingBox2d.singleton (toPoint vertex))
                        , children = ( vertex, first :: rest )
                        , aggregate = ()
                        }

        Node node ->
            let
                point =
                    toPoint vertex
            in
                if BoundingBox2d.contains point node.boundingBox then
                    case quadrant node.boundingBox (toPoint vertex) of
                        NE ->
                            Node { node | ne = insertBy toPoint vertex node.ne }

                        SE ->
                            Node { node | se = insertBy toPoint vertex node.se }

                        NW ->
                            Node { node | nw = insertBy toPoint vertex node.nw }

                        SW ->
                            Node { node | sw = insertBy toPoint vertex node.sw }
                else
                    let
                        { minX, minY, maxX, maxY } =
                            BoundingBox2d.extrema node.boundingBox

                        ( width, height ) =
                            BoundingBox2d.dimensions node.boundingBox
                    in
                        case quadrant node.boundingBox (toPoint vertex) of
                            NE ->
                                Node
                                    { boundingBox = BoundingBox2d.with { minX = minX, maxX = maxX + width, minY = minY, maxY = maxY + height }
                                    , ne = singleton toPoint vertex
                                    , sw = qtree
                                    , se = Empty
                                    , nw = Empty
                                    , aggregate = ()
                                    }

                            SE ->
                                Node
                                    { boundingBox = BoundingBox2d.with { maxY = maxY, minX = minX, maxX = maxX + width, minY = minY - height }
                                    , se = singleton toPoint vertex
                                    , nw = qtree
                                    , sw = Empty
                                    , ne = Empty
                                    , aggregate = ()
                                    }

                            NW ->
                                Node
                                    { boundingBox = BoundingBox2d.with { maxX = maxX, minY = minY, maxY = maxY + height, minX = minX - width }
                                    , nw = singleton toPoint vertex
                                    , se = qtree
                                    , sw = Empty
                                    , ne = Empty
                                    , aggregate = ()
                                    }

                            SW ->
                                Node
                                    { boundingBox = BoundingBox2d.with { maxX = maxX, maxY = maxY, minX = minX - width, minY = minY - height }
                                    , sw = singleton toPoint vertex
                                    , ne = qtree
                                    , se = Empty
                                    , nw = Empty
                                    , aggregate = ()
                                    }


type Quadrant
    = NE
    | NW
    | SE
    | SW


quadrant : BoundingBox2d -> Point2d -> Quadrant
quadrant boundingBox point =
    let
        { minX, minY, maxX, maxY } =
            BoundingBox2d.extrema boundingBox

        ( midX, midY ) =
            BoundingBox2d.centroid boundingBox |> Point2d.coordinates

        ( x, y ) =
            Point2d.coordinates point
    in
        if y >= midY then
            if x >= midX then
                NE
            else
                NW
        else if x >= midX then
            SE
        else
            SW


fromList : (vertex -> Point2d) -> List vertex -> AggregateQuadTree () vertex
fromList toPoint =
    List.foldl (insertBy toPoint) empty


toList : AggregateQuadTree x a -> List a
toList qtree =
    case qtree of
        Empty ->
            []

        Leaf leaf ->
            let
                ( first, rest ) =
                    leaf.children
            in
                first :: rest

        Node node ->
            toList node.nw ++ toList node.ne ++ toList node.se ++ toList node.sw


aggregate : Config aggregate vertex -> AggregateQuadTree x vertex -> AggregateQuadTree aggregate vertex
aggregate ({ combineAggregates, combineVertices } as config) vanillaQuadTree =
    case vanillaQuadTree of
        Empty ->
            Empty

        Leaf leaf ->
            let
                ( first, rest ) =
                    leaf.children
            in
                Leaf
                    { boundingBox = leaf.boundingBox
                    , children = ( first, rest )
                    , aggregate = combineVertices first rest
                    }

        Node node ->
            let
                newNw =
                    aggregate config node.nw

                newSw =
                    aggregate config node.sw

                newNe =
                    aggregate config node.ne

                newSe =
                    aggregate config node.se

                subresults =
                    List.filterMap identity
                        [ getAggregate newNw
                        , getAggregate newSw
                        , getAggregate newNe
                        , getAggregate newSe
                        ]
            in
                case subresults of
                    [] ->
                        Empty

                    x :: xs ->
                        Node
                            { boundingBox = node.boundingBox
                            , aggregate = combineAggregates x xs
                            , nw = newNw
                            , sw = newSw
                            , ne = newNe
                            , se = newSe
                            }


getAggregate : AggregateQuadTree aggregate vertex -> Maybe aggregate
getAggregate qtree =
    case qtree of
        Empty ->
            Nothing

        Leaf { aggregate } ->
            Just aggregate

        Node { aggregate } ->
            Just aggregate
