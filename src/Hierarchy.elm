module Hierarchy exposing (Attribute, Supported, TilingMethod, dice, layered, nodeSize, none, padding, paddingInner, paddingOuter, parentChildMargin, partition, peerMargin, preserveAspectRatio, size, slice, sliceDice, squarify, squarifyRatio, tidy, tile, treemap)

import Hierarchy.Partition
import Hierarchy.Tidy
import Hierarchy.Tree as Tree exposing (Tree)
import Hierarchy.Treemap


type Supported
    = Supported


processAttributes : (Attribute a attr -> b -> b) -> b -> List (Attribute a attr) -> b
processAttributes assigner =
    List.foldl
        (\a d ->
            case a of
                Batch l ->
                    processAttributes assigner d l

                _ ->
                    assigner a d
        )


type Attribute a attr
    = Size Float Float
    | Layered
    | PreserveAspectRatio
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


size : Float -> Float -> Attribute a { b | size : Supported }
size =
    Size


layered : Attribute a { b | layered : Supported }
layered =
    Layered


parentChildMargin : Float -> Attribute a { b | parentChildMargin : Supported }
parentChildMargin =
    ParentChildMargin


peerMargin : Float -> Attribute a { b | peerMargin : Supported }
peerMargin =
    PeerMargin


nodeSize : (a -> ( Float, Float )) -> Attribute a { b | nodeSize : Supported }
nodeSize =
    NodeSize


paddingInner : (a -> Float) -> Attribute a { b | paddingInner : Supported }
paddingInner =
    PaddingInner


paddingOuter : (a -> Float) -> Attribute a { b | paddingOuter : Supported }
paddingOuter f =
    Batch [ PaddingLeft f, PaddingRight f, PaddingBottom f, PaddingTop f ]


padding : (a -> Float) -> Attribute a { b | padding : Supported }
padding =
    Padding


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


preserveAspectRatio : Attribute a { b | preserveAspectRatio : Supported }
preserveAspectRatio =
    PreserveAspectRatio


tidy :
    List
        (Attribute
            a
            { size : Supported
            , nodeSize : Supported
            , layered : Supported
            , parentChildMargin : Supported
            , peerMargin : Supported
            , preserveAspectRatio : Supported
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
                            ( s, { r | nodeSize = ns }, a )

                        PreserveAspectRatio ->
                            ( s, r, True )

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
                    { l
                        | x = (l.x - minX_) * wS
                        , width = l.width * wS
                        , height = l.height * hS
                        , y = l.y * hS
                    }
                )
                layout

        Nothing ->
            layout


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


type alias TilingMethod =
    Int
    -> { x0 : Float, x1 : Float, y0 : Float, y1 : Float }
    -> Float
    -> List Float
    -> List { x0 : Float, x1 : Float, y0 : Float, y1 : Float }


slice : TilingMethod
slice =
    Hierarchy.Treemap.slice


dice : TilingMethod
dice =
    Hierarchy.Treemap.dice


sliceDice : TilingMethod
sliceDice =
    Hierarchy.Treemap.sliceDice


squarify : TilingMethod
squarify =
    Hierarchy.Treemap.squarify


squarifyRatio : Float -> TilingMethod
squarifyRatio =
    Hierarchy.Treemap.squarifyRatio


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
