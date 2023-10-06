module Hierarchy.TreemapTests exposing (suite)

import Expect
import Fuzz
import Hierarchy
import HierarchyTests exposing (fuzzTree)
import Test exposing (Test)
import Tree


fuzzSettings =
    Fuzz.map8
        (\pi pl pr pt pb t w h ->
            { paddingInner = pi
            , paddingLeft = pl
            , paddingRight = pr
            , paddingTop = pt
            , paddingBottom = pb
            , tile = t
            , width = w
            , height = h
            }
        )
        (Fuzz.floatAtLeast 0)
        (Fuzz.floatAtLeast 0)
        (Fuzz.floatAtLeast 0)
        (Fuzz.floatAtLeast 0)
        (Fuzz.floatAtLeast 0)
        (Fuzz.oneOfValues [ Squarify, SlideDice, Slice, Dice ])
        (Fuzz.floatRange 1 100)
        (Fuzz.floatRange 1 100)


simple =
    Tree.tree 24
        [ Tree.singleton 6
        , Tree.singleton 6
        , Tree.singleton 4
        , Tree.singleton 3
        , Tree.singleton 2
        , Tree.singleton 2
        , Tree.singleton 1
        ]


type TilingMethod
    = Squarify
    | SlideDice
    | Slice
    | Dice


suite : Test
suite =
    Test.describe "treemap"
        [ Test.fuzz2
            (Fuzz.map
                (Tree.restructure identity
                    (\l c ->
                        case c of
                            [] ->
                                Tree.singleton l

                            _ ->
                                Tree.tree (c |> List.map Tree.label |> List.sum) c
                    )
                )
                (fuzzTree (Fuzz.intAtLeast 1))
            )
            fuzzSettings
            "tree areas are proportional to their value"
          <|
            \t set ->
                let
                    layout =
                        t
                            |> Hierarchy.treemap
                                [ Hierarchy.tile <|
                                    case set.tile of
                                        Squarify ->
                                            Hierarchy.squarify

                                        SlideDice ->
                                            Hierarchy.sliceDice

                                        Slice ->
                                            Hierarchy.slice

                                        Dice ->
                                            Hierarchy.dice
                                , Hierarchy.size set.width set.height
                                ]
                                toFloat
                            |> Tree.map (\l -> l.width * l.height)

                    total =
                        Tree.label t |> toFloat

                    totalArea =
                        Tree.label layout

                    factor =
                        total / totalArea

                    input =
                        Tree.toList t

                    output =
                        Tree.toList layout
                in
                output
                    |> List.map (\v -> round (v * factor))
                    |> Expect.equal input
        , Test.test "simple tree" <|
            \() ->
                simple
                    |> Hierarchy.treemap [ Hierarchy.size 6 4 ] identity
                    |> Tree.toList
                    |> List.map toCoords
                    |> Expect.equal
                        [ { x0 = 0.0, x1 = 6.0, y0 = 0.0, y1 = 4.0 }
                        , { x0 = 0.0, x1 = 3.0, y0 = 0.0, y1 = 2.0 }
                        , { x0 = 0.0, x1 = 3.0, y0 = 2.0, y1 = 4.0 }
                        , { x0 = 3.0, x1 = 4.71, y0 = 0.0, y1 = 2.33 }
                        , { x0 = 4.71, x1 = 6.0, y0 = 0.0, y1 = 2.33 }
                        , { x0 = 3.0, x1 = 5.4, y0 = 2.33, y1 = 3.17 }
                        , { x0 = 3.0, x1 = 5.4, y0 = 3.17, y1 = 4.0 }
                        , { x0 = 5.4, x1 = 6.0, y0 = 2.33, y1 = 4.0 }
                        ]
        , Test.test "simple tree observes inner padding" <|
            \() ->
                simple
                    |> Hierarchy.treemap [ Hierarchy.paddingInner (always 0.5), Hierarchy.size 6 4 ] identity
                    |> Tree.toList
                    |> List.map toCoords
                    |> Expect.equal
                        [ { x0 = 0.0, x1 = 6.0, y0 = 0.0, y1 = 4.0 }
                        , { x0 = 0.0, x1 = 2.75, y0 = 0.0, y1 = 1.75 }
                        , { x0 = 0.0, x1 = 2.75, y0 = 2.25, y1 = 4.0 }
                        , { x0 = 3.25, x1 = 4.61, y0 = 0.0, y1 = 2.13 }
                        , { x0 = 5.11, x1 = 6.0, y0 = 0.0, y1 = 2.13 }
                        , { x0 = 3.25, x1 = 5.35, y0 = 2.63, y1 = 3.06 }
                        , { x0 = 3.25, x1 = 5.35, y0 = 3.56, y1 = 4.0 }
                        , { x0 = 5.85, x1 = 6.0, y0 = 2.63, y1 = 4.0 }
                        ]
        , Test.test "simple tree with slice" <|
            \() ->
                simple
                    |> Hierarchy.treemap [ Hierarchy.tile Hierarchy.slice, Hierarchy.size 6 4 ] identity
                    |> Tree.toList
                    |> List.map toCoords
                    |> Expect.equal
                        [ { x0 = 0.0, x1 = 6.0, y0 = 0.0, y1 = 4.0 }
                        , { x0 = 0.0, x1 = 6.0, y0 = 0.0, y1 = 1.0 }
                        , { x0 = 0.0, x1 = 6.0, y0 = 1.0, y1 = 2.0 }
                        , { x0 = 0.0, x1 = 6.0, y0 = 2.0, y1 = 2.67 }
                        , { x0 = 0.0, x1 = 6.0, y0 = 2.67, y1 = 3.17 }
                        , { x0 = 0.0, x1 = 6.0, y0 = 3.17, y1 = 3.5 }
                        , { x0 = 0.0, x1 = 6.0, y0 = 3.5, y1 = 3.83 }
                        , { x0 = 0.0, x1 = 6.0, y0 = 3.83, y1 = 4.0 }
                        ]
        ]


roundF : Float -> Float
roundF f =
    toFloat (round (f * 100)) / 100


toCoords t =
    { x0 = roundF t.x
    , y0 = roundF t.y
    , x1 = roundF (t.x + t.width)
    , y1 = roundF (t.y + t.height)
    }
