module Hierarchy exposing
    ( Attribute, Supported, none
    , tidy, partition, treemap
    , size, nodeSize, layered, parentChildMargin, peerMargin, padding, paddingOuter, paddingInner, paddingTop, paddingBottom, paddingLeft, paddingRight, tile
    , slice, dice, sliceDice, squarify, squarifyRatio, TilingMethod
    )

{-| Hierarchical data has its own visualization requirements, since usually the parent-child relationships
tend to be important in understanding the dataset.

This module implements several layout techniques for visualizing such data.


## Basic types


### `Tree a`

The tree type used here comes from the gampleman/elm-rosetree package, which is a fully featured and performant
library fro dealing with trees. It has several methods you can use to convert other datastructures you may have
into that format and use it for visualizing your data.

@docs Attribute, Supported, none


### Return types

Most of the layouts return a record with `x`, `y`, `width` and `height` attributes. Naturally the simplest is
to take these values literally and simply produce a rectangle with these properties. However, these can be
profitably interpreted abstractly. For instance one may produce a horizontal diagram by simply switching `x` with
`y` and `width` with `height`. Or treat `x` and `x + width` as angles in a radial layout.


# Layouts

@docs tidy, partition, treemap


# Options

@docs size, nodeSize, layered, parentChildMargin, peerMargin, padding, paddingOuter, paddingInner, paddingTop, paddingBottom, paddingLeft, paddingRight, tile


## Tiling methods

@docs slice, dice, sliceDice, squarify, squarifyRatio, TilingMethod

-}

import Hierarchy.Partition
import Hierarchy.Tidy
import Hierarchy.Treemap
import Tree exposing (Tree)


{-| Used to indicate which attributes go with which layout functions.

For instance, `padding` returns :

    Attribute a { b | padding = Supported }

now `partition` takes as its first argument:

    List (Attribute a { padding = Supported, size = Supported })

The compiler is quite happy to unify these types, but for instance
passing this to `tidy` would cause a type error. It also makes it quite
easy to understand from the type signature which options are supported.

-}
type Supported
    = Supported Never


processAttributes : (Attribute a attr -> b -> b) -> b -> List (Attribute a attr) -> b
processAttributes assigner =
    -- IGNORE TCO
    List.foldl
        (\a d ->
            case a of
                Batch l ->
                    processAttributes assigner d l

                _ ->
                    assigner a d
        )


{-| Each of the layout functions can be customized using a number of optional
arguments; these are represented by this type.

The first type argument represents the data contained in your tree, the second
is a phantom type that ensures only valid options are passed to each layout
method.

-}
type Attribute a attr
    = Size Float Float
    | Layered
    | ParentChildMargin Float
    | PeerMargin Float
    | NodeSize (a -> ( Float, Float ))
    | Padding (a -> Float)
    | PaddingInner (a -> Float)
    | PaddingLeft (a -> Float)
    | PaddingRight (a -> Float)
    | PaddingTop (a -> Float)
    | PaddingBottom (a -> Float)
    | Tile TilingMethod
    | Batch (List (Attribute a attr))


{-| Sets the size of the entire layout. For most layouts omitting this option will cause it to have a default size of 1.
-}
size : Float -> Float -> Attribute a { b | size : Supported }
size =
    Size


{-| [![Layered behavior](https://elm-visualization.netlify.com/LayeredTree/preview@2x.png)](https://elm-visualization.netlify.com/LayeredTree/)

Passing this option causes each "layer" (i.e. nodes in the tree that have the same number of ancestor nodes)
to be layed out with the same y value. This makes the layers much more emphasized (if you are for instance
visualizing the organization of an army unit, then this might neatly show the rank of each member) at the cost
of needing more space.

This only makes a difference if `nodeSize` returns different heights for different children.

-}
layered : Attribute a { b | layered : Supported }
layered =
    Layered


{-| The vertical distance between a parent and a child in a tree.
-}
parentChildMargin : Float -> Attribute a { b | parentChildMargin : Supported }
parentChildMargin =
    ParentChildMargin


{-| The horizontal distance between nodes layed out next to each other.
-}
peerMargin : Float -> Attribute a { b | peerMargin : Supported }
peerMargin =
    PeerMargin


{-| Sets the size of the actual node to be layed out. This will be the actual
size if the `size` option isn't passed, otherwise this size will get proportionally
scaled (preserving aspect ratio).

The default size of a node is `( 1, 1 )`.

-}
nodeSize : (a -> ( Float, Float )) -> Attribute a { b | nodeSize : Supported }
nodeSize =
    NodeSize


{-| The inner padding is used to separate a node’s adjacent children.
-}
paddingInner : (a -> Float) -> Attribute a { b | paddingInner : Supported }
paddingInner =
    PaddingInner


{-| Sets `paddingLeft`, `paddingRight`, `paddingTop` and `paddingBottom` in one go.
-}
paddingOuter : (a -> Float) -> Attribute a { b | paddingOuter : Supported }
paddingOuter f =
    Batch [ PaddingLeft f, PaddingRight f, PaddingBottom f, PaddingTop f ]


{-| The top padding is used to separate the top edge of a node from its children.
-}
paddingTop : (a -> Float) -> Attribute a { b | paddingOuter : Supported }
paddingTop =
    PaddingTop


{-| The bottom padding is used to separate the bottom edge of a node from its children.
-}
paddingBottom : (a -> Float) -> Attribute a { b | paddingOuter : Supported }
paddingBottom =
    PaddingBottom


{-| The left padding is used to separate the left edge of a node from its children.
-}
paddingLeft : (a -> Float) -> Attribute a { b | paddingOuter : Supported }
paddingLeft =
    PaddingLeft


{-| The right padding is used to separate the right edge of a node from its children.
-}
paddingRight : (a -> Float) -> Attribute a { b | paddingOuter : Supported }
paddingRight =
    PaddingRight


{-| Sets the distances between nodes. For treemaps, this is a shortcut to setting both paddingInner and paddingOuter.
-}
padding : (a -> Float) -> Attribute a { b | padding : Supported }
padding =
    Padding


{-| Sets the tiling method to be used. The default is `squarify`.
-}
tile : TilingMethod -> Attribute a { b | tile : Supported }
tile =
    Tile


{-| Attribute that doesn't affect the settings at all. Can be useful when settings are produced conditionally:

    [ Hierarchy.size 230 520
    , if doLayered then
        Hierarchy.layered

      else
        Hierarchy.none
    ]

-}
none : Attribute a b
none =
    Batch []


{-| Produces a tidy node-link diagram of a tree, based on a linear time algorithm by [van der Ploeg](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=d45f66231e053590c64c9d901fb7b028dbc5c923).

[![Tidy Tree](https://elm-visualization.netlify.com/TidyTree/preview@2x.png)](https://elm-visualization.netlify.com/TidyTree/)

-}
tidy :
    List
        (Attribute
            a
            { size : Supported
            , nodeSize : Supported
            , layered : Supported
            , parentChildMargin : Supported
            , peerMargin : Supported
            }
        )
    -> Tree a
    -> Tree { height : Float, node : a, width : Float, x : Float, y : Float }
tidy attrs t =
    let
        ( resize, settings, aspectRatio ) =
            processAttributes
                (\attr ( s, r, a ) ->
                    case attr of
                        Size x y ->
                            ( Just ( x, y ), r, a )

                        Layered ->
                            ( s, { r | layered = True }, a )

                        ParentChildMargin m ->
                            ( s, { r | parentChildMargin = m }, a )

                        PeerMargin m ->
                            ( s, { r | peerMargin = m }, a )

                        NodeSize ns ->
                            ( s, { r | nodeSize = ns }, True )

                        _ ->
                            ( s, r, a )
                )
                ( Nothing
                , { nodeSize = always ( 1, 1 )
                  , layered = False
                  , parentChildMargin = 1
                  , peerMargin = 1
                  }
                , False
                )
                attrs

        layout =
            Hierarchy.Tidy.layout settings t
    in
    case resize of
        Just ( w, h ) ->
            let
                ( minX_, maxX_, height ) =
                    Tree.foldl
                        (\l ( minX, maxX, maxY ) ->
                            ( min minX l.x, max maxX (l.x + l.width), max maxY (l.y + l.height) )
                        )
                        ( 0, 0, 0 )
                        layout

                width =
                    maxX_ - minX_

                wS_ =
                    w / width

                hS_ =
                    h / height

                ( wS, hS ) =
                    if aspectRatio then
                        ( min wS_ hS_, min wS_ hS_ )

                    else
                        ( wS_, hS_ )
            in
            Tree.map
                (\l ->
                    if aspectRatio then
                        { l
                            | x = (l.x - minX_) * wS
                            , width = l.width * wS
                            , height = l.height * hS
                            , y = l.y * hS
                        }

                    else
                        -- we have size but no nodeSize, so we want to keep nodes at (1,1)
                        -- as such we reposition them to me in the middle of the newly scaled box
                        { l
                            | x = (l.x - minX_ + l.width / 2) * wS - 0.5
                            , width = 1
                            , height = 1
                            , y = (l.y + l.height / 2) * hS - 0.5
                        }
                )
                layout

        Nothing ->
            layout


{-| A treemap recursively subdivides area into rectangles according to each node’s associated value. This implementation supports an extensible tiling method.

[![Treemap](https://elm-visualization.netlify.com/Treemap/preview@2x.png)](https://elm-visualization.netlify.com/Treemap/)

-}
treemap :
    List
        (Attribute
            a
            { padding : Supported
            , paddingInner : Supported
            , paddingOuter : Supported
            , tile : Supported
            , size : Supported
            }
        )
    -> (a -> Float)
    -> Tree a
    -> Tree { x : Float, y : Float, width : Float, height : Float, value : Float, node : a }
treemap attrs value =
    Hierarchy.Treemap.layout
        (processAttributes
            (\attr d ->
                case attr of
                    Size w h ->
                        { d | size = ( w, h ) }

                    PaddingInner p ->
                        { d | paddingInner = p }

                    PaddingLeft p ->
                        { d | paddingLeft = p }

                    PaddingRight p ->
                        { d | paddingRight = p }

                    PaddingTop p ->
                        { d | paddingTop = p }

                    PaddingBottom p ->
                        { d | paddingBottom = p }

                    Padding p ->
                        { d | paddingInner = p, paddingLeft = p, paddingRight = p, paddingTop = p, paddingBottom = p }

                    Tile t ->
                        { d | tile = t }

                    _ ->
                        d
            )
            { paddingInner = always 0
            , paddingLeft = always 0
            , paddingRight = always 0
            , paddingTop = always 0
            , paddingBottom = always 0
            , tile = squarify
            , value = value
            , size = ( 1, 1 )
            }
            attrs
        )


{-| You can implement your own tiling method as it's just a function. It recieves the following arguments:

  - depth: how many ancestors does the current node have
  - the bounding box of the current node
  - the quantitative value of the current node
  - the values of all the children nodes

It is expected to return the bounding boxes of the children.

For example, slice can be implemented like this (slightly simplified):

    slice : TilingMethod
    slice _ { x0, x1, y0, y1 } value children =
        List.foldl
            (\childValue ( prevY, lst ) ->
                let
                    nextY =
                        prevY + childValue * ((y1 - y0) / value)
                in
                ( nextY, { x0 = x0, x1 = x1, y0 = prevY, y1 = nextY } :: lst )
            )
            ( y0, [] )
            children
            |> Tuple.second
            |> List.reverse

Note that padding and such will be applied later.

-}
type alias TilingMethod =
    Int
    -> { x0 : Float, x1 : Float, y0 : Float, y1 : Float }
    -> Float
    -> List Float
    -> List { x0 : Float, x1 : Float, y0 : Float, y1 : Float }


{-| Divides the rectangular area **vertically**. The children are positioned in order, starting with the top edge (y0) of the given rectangle.

If the sum of the children’s values is less than the specified node’s value (i.e., if the specified node has a non-zero internal value), the remaining empty space will be positioned on the bottom edge (y1) of the given rectangle.

-}
slice : TilingMethod
slice =
    Hierarchy.Treemap.slice


{-| Divides the rectangular area **horizontally**. The children are positioned in order, starting with the left edge (x0) of the given rectangle.
-}
dice : TilingMethod
dice =
    Hierarchy.Treemap.dice


{-| If the depth is odd, delegates to `slice`; otherwise delegates to `dice`.
-}
sliceDice : TilingMethod
sliceDice =
    Hierarchy.Treemap.sliceDice


{-| Implements the squarified treemap algorithm by [Bruls et al.](https://www.win.tue.nl/~vanwijk/stm.pdf), which seeks to produce rectangles of a given aspect ratio, in this case the golden ratio φ = (1 + sqrt(5)) / 2.
-}
squarify : TilingMethod
squarify =
    Hierarchy.Treemap.squarify


{-| Implements the squarified treemap algorithm by [Bruls et al.](https://www.win.tue.nl/~vanwijk/stm.pdf), which seeks to produce rectangles of the given aspect ratio. The ratio must be specified as a number greater than or equal to one. Note that the orientation of the generated rectangles (tall or wide) is not implied by the ratio; for example, a ratio of two will attempt to produce a mixture of rectangles whose width:height ratio is either 2:1 or 1:2. (However, you can approximately achieve this result by generating a square treemap at different dimensions, and then stretching the treemap to the desired aspect ratio.) Furthermore, the specified ratio is merely a hint to the tiling algorithm; the rectangles are not guaranteed to have the specified aspect ratio.
-}
squarifyRatio : Float -> TilingMethod
squarifyRatio =
    Hierarchy.Treemap.squarifyRatio


{-| The partition layout produces adjacency diagrams: a space-filling variant of a node-link tree diagram. Rather than drawing a link between parent and child in the hierarchy, nodes are drawn as solid areas (either arcs or rectangles), and their placement relative to other nodes reveals their position in the hierarchy. The size of the nodes encodes a quantitative dimension that would be difficult to show in a node-link diagram.

[![Sunburst diagram](https://elm-visualization.netlify.com/Sunburst/preview@2x.png)](https://elm-visualization.netlify.com/Sunburst/)

-}
partition :
    List (Attribute a { padding : Supported, size : Supported })
    -> (a -> Float)
    -> Tree a
    -> Tree { x : Float, y : Float, width : Float, height : Float, value : Float, node : a }
partition attrs value =
    Hierarchy.Partition.layout
        (processAttributes
            (\attr d ->
                case attr of
                    Size w h ->
                        { d | size = ( w, h ) }

                    Padding p ->
                        { d | padding = p }

                    _ ->
                        d
            )
            { padding = always 0
            , value = value
            , size = ( 1, 1 )
            }
            attrs
        )
