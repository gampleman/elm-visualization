module Hierarchy.Tree exposing
    ( Tree, singleton, tree, label, children
    , mapLabel, replaceLabel, mapChildren, replaceChildren, prependChild, appendChild
    , count, depth
    , foldl, foldr
    , flatten, leaves, links
    , map, indexedMap, mapAccumulate, map2, indexedMap2, mapAccumulate2, andMap, mapWithContextBottomUp, mapAccumulateWithContextBottomUp, mapWithContextTopDown, mapAccumulateWithContextTopDown
    , findBfs
    , sortWith, unfold, restructure
    )

{-| A multiway tree or rosetree is a labeled tree where each node can have zero,
one or more children, each of which represents a tree in its own right.

The root of the tree is always labeled, so a tree always has at least one label.

As an example, such a structure could represent a directory structure:

    tree "root"
        [ tree "home"
            [ tree "user1" []
            , tree "user2" []
            ]
        , tree "etc" []
        , tree "var"
            [ tree "log" []
            ]
        ]

In a sense, `Html msg` is pretty similar to how such trees look, but they can be
used to represent other things. A nested menu structure, or a sitemap, or any
other structure where a single root is connected to children which can each have
children of their own, and so on.


# Structure

@docs Tree, singleton, tree, label, children


# Modification

@docs mapLabel, replaceLabel, mapChildren, replaceChildren, prependChild, appendChild


# Describing a tree

@docs count, depth


# Folds

@docs foldl, foldr


# Converting to lists

@docs flatten, leaves, links


# Mapping and traversing

@docs map, indexedMap, mapAccumulate, map2, indexedMap2, mapAccumulate2, andMap, mapWithContextBottomUp, mapAccumulateWithContextBottomUp, mapWithContextTopDown, mapAccumulateWithContextTopDown


# Search

@docs findBfs


# Fancy stuff

@docs sortWith, unfold, restructure

-}


{-| Represents a multiway tree. Each node in the tree holds a piece of
information (the `label`) and a list of children, each of which is a tree.
-}
type Tree a
    = Tree a (List (Tree a))


{-| Creates a singleton tree. This corresponds to `tree v []`.

    singleton 5
        |> label
    --> 5

    singleton "foo"
        |> children
    --> []

-}
singleton : a -> Tree a
singleton v =
    Tree v []


{-| Construct a tree from a label and a list of children.

    tree 5 []
    --> singleton 5


    tree 5
        [ singleton 1
        , singleton 2
        , tree 3
            [ singleton 4
            , singleton 5
            ]
        ]
        |> count
    --> 6

-}
tree : a -> List (Tree a) -> Tree a
tree =
    Tree


{-| Gives you the label of a tree.

    tree "hello" [ singleton "world", singleton "etc" ]
        |> label
    --> "hello"

-}
label : Tree a -> a
label (Tree v _) =
    v


{-| Execute a function on the label of this tree.

    tree "hello" [ singleton "world", singleton "etc" ]
        |> mapLabel String.toUpper
    --> tree "HELLO" [ singleton "world", singleton "etc" ]

-}
mapLabel : (a -> a) -> Tree a -> Tree a
mapLabel f (Tree v cs) =
    Tree (f v) cs


{-| Replace the label of this tree.

    singleton "foo"
        |> replaceLabel "bar"
    --> singleton "bar"

-}
replaceLabel : a -> Tree a -> Tree a
replaceLabel v (Tree _ cs) =
    Tree v cs


{-| Returns the children of a tree as a list.

    singleton "heh"
        |> children
    --> []


    tree "hello" [ singleton "world", singleton "etc" ]
        |> children
    --> [ singleton "world", singleton "etc" ]

-}
children : Tree a -> List (Tree a)
children (Tree _ c) =
    c


{-| Execute a function on the children of a tree.

    tree "lower1"
        [ singleton "upper1"
        , tree "upper2" [ singleton "lower2"]
        , singleton "upper3"
        ]
        |> mapChildren (List.map (mapLabel String.toUpper))
    --> tree "lower1"
    -->     [ singleton "UPPER1"
    -->     , tree "UPPER2" [ singleton "lower2"]
    -->     , singleton "UPPER3"
    -->     ]

-}
mapChildren : (List (Tree a) -> List (Tree a)) -> Tree a -> Tree a
mapChildren f (Tree v cs) =
    Tree v (f cs)


{-| Replace the children of a tree.

    tree "hello" [ singleton "world" ]
        |> replaceChildren [ singleton "everyone" ]
    --> tree "hello" [ singleton "everyone" ]

-}
replaceChildren : List (Tree a) -> Tree a -> Tree a
replaceChildren cs (Tree v _) =
    Tree v cs


{-| Prepend a single child to a tree.

    tree "hello" [ singleton "everyone" ]
        |> prependChild (singleton "dear")
    --> tree "hello" [ singleton "dear", singleton "everyone" ]

-}
prependChild : Tree a -> Tree a -> Tree a
prependChild c (Tree v cs) =
    Tree v (c :: cs)


{-| Append a child to a tree. Note that this uses `children ++ [ newChild ]`
under the hood so use sparingly.

    tree "hello" [ singleton "you" ]
        |> appendChild (singleton "and you!")
    --> tree "hello" [ singleton "you", singleton "and you!" ]

-}
appendChild : Tree a -> Tree a -> Tree a
appendChild c (Tree v cs) =
    Tree v (cs ++ [ c ])


{-| Count the labels in a tree.

    singleton "foo"
        |> count
    --> 1

    tree "foo" [ singleton "bar", singleton "baz" ]
        |> count
    --> 3

-}
count : Tree a -> Int
count t =
    foldl (\_ x -> x + 1) 0 t


{-| Fold over all the labels in a tree, left to right, depth first.

    tree "Hello "
        [ singleton "world "
        , tree "and "
            [ singleton "you "
            , singleton "and "
            , singleton "you"
            ]
        , singleton "!"
        ]
        |> foldl (\label acc -> acc ++ label) ""
    --> "Hello world and you and you!"

-}
foldl : (a -> b -> b) -> b -> Tree a -> b
foldl f acc t =
    foldlHelp f acc [ t ] []


{-| Fold over all the labels in a tree, right to left, depth first.

    tree 1
        [ singleton 2
        , tree 3
            [ singleton 4
            , singleton 5
            ]
        , singleton 6
        ]
        |> foldr (::) []
    --> [ 1, 2, 3, 4, 5, 6 ]

-}
foldr : (a -> b -> b) -> b -> Tree a -> b
foldr f acc t =
    List.foldl f acc <| foldl (::) [] t


foldlHelp : (a -> b -> b) -> b -> List (Tree a) -> List (List (Tree a)) -> b
foldlHelp f acc trees nextSets =
    case trees of
        [] ->
            case nextSets of
                set :: sets ->
                    foldlHelp f acc set sets

                [] ->
                    acc

        (Tree d []) :: rest ->
            foldlHelp f (f d acc) rest nextSets

        (Tree d xs) :: rest ->
            foldlHelp f (f d acc) xs (rest :: nextSets)


{-| Flattens the tree into a list. This is equivalent to `foldr (::) []`
-}
flatten : Tree a -> List a
flatten t =
    foldr (::) [] t


{-| Returns the nodes that have no children.
-}
leaves : Tree a -> List a
leaves t =
    leavesHelp [] [ t ] []


leavesHelp : List a -> List (Tree a) -> List (List (Tree a)) -> List a
leavesHelp soFar trees nextSets =
    case trees of
        [] ->
            case nextSets of
                set :: sets ->
                    leavesHelp soFar set sets

                [] ->
                    soFar

        (Tree d []) :: rest ->
            leavesHelp (d :: soFar) rest nextSets

        (Tree _ xs) :: rest ->
            leavesHelp soFar rest (xs :: nextSets)


{-| Returns pairs representing parent-child relationships in the tree.

The left item is the label of the parent, the right item is the label of
the child. Useful for visualising trees.

    tree 1
        [ singleton 2
        , tree 3
            [ singleton 4
            , singleton 5
            ]
        , singleton 6
        ]
        |> links
    --> [ ( 1, 2 ), ( 1, 3 ), ( 1, 6 ), ( 3, 4 ), ( 3, 5 ) ]

-}
links : Tree a -> List ( a, a )
links (Tree l cr) =
    linksHelp l [] cr []


linksHelp : a -> List ( a, a ) -> List (Tree a) -> List ( a, List (Tree a) ) -> List ( a, a )
linksHelp parent soFar trees nextSets =
    case trees of
        [] ->
            case nextSets of
                ( newParent, set ) :: sets ->
                    linksHelp newParent soFar set sets

                [] ->
                    List.reverse soFar

        (Tree d []) :: rest ->
            linksHelp parent (( parent, d ) :: soFar) rest nextSets

        (Tree d xs) :: rest ->
            linksHelp parent (( parent, d ) :: soFar) rest (( d, xs ) :: nextSets)


{-| Create a tree from a seed.

Running the function on the seed should return a label and a list of seeds to
use for the children.

For example, this function takes and int, and uses the string representation of
that int as the label, with its children representing the integers from 0 up to
but not including the value. The expected result is a tree in which each label
has the number of children mentioned in the label, recursively.

    unfolder : Int -> (String, List Int)
    unfolder x =
        ( String.fromInt x, List.range 0 (x - 1) )


    unfold unfolder 3
    --> tree "3"
    -->     [ singleton "0"
    -->     , tree "1" [ singleton "0" ]
    -->     , tree "2"
    -->         [ singleton "0"
    -->         , tree "1" [ singleton "0" ]
    -->         ]
    -->     ]

-}
unfold : (b -> ( a, List b )) -> b -> Tree a
unfold f seed =
    let
        ( v, next ) =
            f seed
    in
    unfoldHelp f { todo = next, label = v, done = [] } []


unfoldHelp :
    (b -> ( a, List b ))
    -> UnfoldAcc a b
    -> List (UnfoldAcc a b)
    -> Tree a
unfoldHelp f acc stack =
    case acc.todo of
        [] ->
            let
                node =
                    Tree acc.label (List.reverse acc.done)
            in
            case stack of
                [] ->
                    node

                top :: rest ->
                    unfoldHelp f
                        { top | done = node :: top.done }
                        rest

        x :: xs ->
            case f x of
                ( label_, [] ) ->
                    unfoldHelp f
                        { acc
                            | todo = xs
                            , done = singleton label_ :: acc.done
                        }
                        stack

                ( label_, todo ) ->
                    unfoldHelp f
                        { todo = todo
                        , label = label_
                        , done = []
                        }
                        ({ acc | todo = xs } :: stack)


type alias UnfoldAcc a b =
    { todo : List b
    , done : List (Tree a)
    , label : a
    }


{-| Run a function on every label in the tree.

    tree 1
        [ singleton 2
        , tree 3 [ singleton 4 ]
        , singleton 5
        ]
        |> map (\x -> String.fromInt (x * 2))
    --> tree "2"
    -->     [ singleton "4"
    -->     , tree "6" [ singleton "8" ]
    -->     , singleton "10"
    -->     ]

-}
map : (a -> b) -> Tree a -> Tree b
map f t =
    mapAccumulate (\_ e -> ( (), f e )) () t
        |> Tuple.second


{-| Run a function on every label in the tree while getting access to the
"index" of the label. This looks at thing in the same order as `foldl`.

    tree "foo"
        [ singleton "bar"
        , tree "baz" [ singleton "hello", singleton "world" ]
        , singleton "qlux"
        ]
        |> indexedMap (\idx val -> String.fromInt idx ++ " - " ++ val)
    --> tree "0 - foo"
    -->     [ singleton "1 - bar"
    -->     , tree "2 - baz"
    -->         [ singleton "3 - hello"
    -->         , singleton "4 - world"
    -->         ]
    -->     , singleton "5 - qlux"
    -->     ]

-}
indexedMap : (Int -> a -> b) -> Tree a -> Tree b
indexedMap f t =
    mapAccumulate (\idx elem -> ( idx + 1, f idx elem )) 0 t
        |> Tuple.second


{-| Map a function over every node while accumulating some value.

    tree 1
        [ singleton 2
        , tree 3 [ singleton 4 ]
        ]
        |> mapAccumulate (\acc label -> ( acc + label, String.fromInt label)) 0
    --> ( 10
    --> , tree "1"
    -->     [ singleton "2"
    -->     , tree "3" [ singleton "4" ]
    -->     ]
    --> )

-}
mapAccumulate : (s -> a -> ( s, b )) -> s -> Tree a -> ( s, Tree b )
mapAccumulate f s (Tree d cs) =
    let
        ( s_, d_ ) =
            f s d
    in
    mapAccumulateHelp f
        s_
        { todo = cs
        , done = []
        , label = d_
        }
        []


mapAccumulateHelp :
    (s -> a -> ( s, b ))
    -> s
    -> MapAcc a b
    -> List (MapAcc a b)
    -> ( s, Tree b )
mapAccumulateHelp f state acc stack =
    case acc.todo of
        [] ->
            let
                node =
                    Tree acc.label (List.reverse acc.done)
            in
            case stack of
                [] ->
                    ( state, node )

                top :: rest ->
                    mapAccumulateHelp f state { top | done = node :: top.done } rest

        (Tree d []) :: rest ->
            let
                ( state_, label_ ) =
                    f state d
            in
            mapAccumulateHelp f
                state_
                { acc
                    | todo = rest
                    , done = Tree label_ [] :: acc.done
                }
                stack

        (Tree d cs) :: rest ->
            let
                ( state_, label_ ) =
                    f state d
            in
            mapAccumulateHelp f
                state_
                { todo = cs
                , done = []
                , label = label_
                }
                ({ acc | todo = rest } :: stack)


type alias MapAcc a b =
    { todo : List (Tree a)
    , done : List (Tree b)
    , label : b
    }


{-| Map over 2 trees. Much like `List.map2`, the result will be truncated to the shorter result.

    left : Tree Int
    left =
        tree 3
            [ singleton 5
            , tree 6 [ singleton 12 ]
            , singleton 4
            ]

    right : Tree Int
    right =
        tree 8
            [ tree 5 [ singleton 9 ]
            , singleton 3
            ]


    map2 (\x y -> x + y) left right
    --> tree 11
    -->     [ singleton 10
    -->     , singleton 9
    -->     ]

-}
map2 : (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2 f left right =
    mapAccumulate2 (\s a b -> ( s, f a b )) () left right
        |> Tuple.second


{-| Like `map2`, but with the "index" added as the first argument.
-}
indexedMap2 : (Int -> a -> b -> c) -> Tree a -> Tree b -> Tree c
indexedMap2 f left right =
    mapAccumulate2 (\s a b -> ( s + 1, f s a b )) 0 left right
        |> Tuple.second


{-| Given a tree of functions and a tree of values, applies the functions to the
matching labels in the tree of values, truncating branches to match the common
shape of the trees.
-}
andMap : Tree (a -> b) -> Tree a -> Tree b
andMap =
    map2 (<|)


{-| Allows mapping over 2 trees while also accumulating a value.

    left : Tree Int
    left =
        tree 3
            [ singleton 5
            , tree 6 [ singleton 12 ]
            , singleton 4
            ]

    right : Tree Int
    right =
        tree 8
            [ tree 5 [ singleton 9 ]
            , singleton 3
            ]


    mapAccumulate2 (\sum x y -> ( sum + x + y, x + y )) 0 left right
    --> ( 30
    --> , tree 11
    -->     [ singleton 10
    -->     , singleton 9
    -->     ]
    --> )

-}
mapAccumulate2 : (s -> a -> b -> ( s, c )) -> s -> Tree a -> Tree b -> ( s, Tree c )
mapAccumulate2 f s_ (Tree a xs) (Tree b ys) =
    let
        ( s, z ) =
            f s_ a b
    in
    mapAccumulate2Help f
        s
        { todoL = xs
        , todoR = ys
        , done = []
        , label = z
        }
        []


mapAccumulate2Help :
    (s -> a -> b -> ( s, c ))
    -> s
    -> Map2Acc a b c
    -> List (Map2Acc a b c)
    -> ( s, Tree c )
mapAccumulate2Help f state acc stack =
    case ( acc.todoL, acc.todoR ) of
        ( [], _ ) ->
            let
                node =
                    Tree acc.label (List.reverse acc.done)
            in
            case stack of
                [] ->
                    ( state, node )

                top :: rest ->
                    mapAccumulate2Help f state { top | done = node :: top.done } rest

        ( _, [] ) ->
            let
                node =
                    Tree acc.label (List.reverse acc.done)
            in
            case stack of
                [] ->
                    ( state, node )

                top :: rest ->
                    mapAccumulate2Help f state { top | done = node :: top.done } rest

        ( (Tree a xs) :: restL, (Tree b ys) :: restR ) ->
            let
                ( state_, label_ ) =
                    f state a b
            in
            mapAccumulate2Help f
                state_
                { todoL = xs
                , todoR = ys
                , done = []
                , label = label_
                }
                ({ acc | todoL = restL, todoR = restR } :: stack)


type alias Map2Acc a b c =
    { todoL : List (Tree a)
    , todoR : List (Tree b)
    , done : List (Tree c)
    , label : c
    }


{-| Counts the number of levels in a tree (where the root is 0).

    depth (tree 2 [ tree 1 [tree 0 []]])
    --> 2

-}
depth : Tree a -> Int
depth t =
    Tuple.first (mapAccumulateWithContextTopDown (\s ctx -> ( max s (List.length ctx.ancestors), () )) 0 t)


{-| Maps the tree, but gives the mapping function access to the context of the tree.

This is quite useful when using a tree to model hierarchical relationships, as the
position of the nodes of the tree is often as significant to operations on the tree as
are the values contained.

This is a pre-order traversal of the tree, meaning that the tree is transformed from the
root to the leaves, giving you access to untransformed children and transformed parent nodes.

-}
mapWithContextTopDown : ({ ancestors : List b, node : a, children : List (Tree a) } -> b) -> Tree a -> Tree b
mapWithContextTopDown f t =
    mapAccumulateWithContextTopDown (\_ e -> ( (), f e )) () t
        |> Tuple.second


{-| Maps the tree, but gives the mapping function access to the context of the tree.

This is a post-order traversal of the tree, meaning that the tree is transformed from the
leaves to the root, giving you access to transformed children and untransformed parent nodes.

-}
mapWithContextBottomUp : ({ ancestors : List a, node : a, children : List (Tree b) } -> b) -> Tree a -> Tree b
mapWithContextBottomUp f t =
    mapAccumulateWithContextBottomUp (\_ e -> ( (), f e )) () t
        |> Tuple.second


{-| Like `mapWithContextTopDown`, but with an accumulator argument.
-}
mapAccumulateWithContextTopDown : (s -> { ancestors : List b, node : a, children : List (Tree a) } -> ( s, b )) -> s -> Tree a -> ( s, Tree b )
mapAccumulateWithContextTopDown f s (Tree d cs) =
    let
        ( s_, d_ ) =
            f s { ancestors = [], node = d, children = cs }
    in
    mapAccumulateWithContextTopDownHelp f
        s_
        { todo = cs
        , done = []
        , label = d_
        , parents = [ d_ ]
        }
        []


mapAccumulateWithContextTopDownHelp :
    (s -> { ancestors : List b, node : a, children : List (Tree a) } -> ( s, b ))
    -> s
    -> MapAccCtxTopDown a b
    -> List (MapAccCtxTopDown a b)
    -> ( s, Tree b )
mapAccumulateWithContextTopDownHelp f state acc stack =
    case acc.todo of
        [] ->
            let
                node =
                    Tree acc.label (List.reverse acc.done)
            in
            case stack of
                [] ->
                    ( state, node )

                top :: rest ->
                    mapAccumulateWithContextTopDownHelp f state { top | done = node :: top.done } rest

        (Tree d []) :: rest ->
            let
                ( state_, label_ ) =
                    f state { children = [], node = d, ancestors = acc.parents }
            in
            mapAccumulateWithContextTopDownHelp f
                state_
                { acc
                    | todo = rest
                    , done = Tree label_ [] :: acc.done
                }
                stack

        (Tree d cs) :: rest ->
            let
                ( state_, label_ ) =
                    f state { children = cs, node = d, ancestors = acc.parents }
            in
            mapAccumulateWithContextTopDownHelp f
                state_
                { todo = cs
                , done = []
                , label = label_
                , parents = label_ :: acc.parents
                }
                ({ acc | todo = rest } :: stack)


type alias MapAccCtxTopDown a b =
    { todo : List (Tree a)
    , done : List (Tree b)
    , parents : List b
    , label : b
    }


{-| Like `mapWithContextBottomUp`, but with an accumulator argument.
-}
mapAccumulateWithContextBottomUp : (s -> { ancestors : List a, node : a, children : List (Tree b) } -> ( s, b )) -> s -> Tree a -> ( s, Tree b )
mapAccumulateWithContextBottomUp f s (Tree d cs) =
    mapAccumulateWithContextBottomUpHelp f
        s
        { todo = cs
        , done = []
        , label = d
        , parents = [ d ]
        }
        []


mapAccumulateWithContextBottomUpHelp :
    (s -> { ancestors : List a, node : a, children : List (Tree b) } -> ( s, b ))
    -> s
    -> MapAccCtxBottomUp a b
    -> List (MapAccCtxBottomUp a b)
    -> ( s, Tree b )
mapAccumulateWithContextBottomUpHelp f state acc stack =
    case acc.todo of
        [] ->
            let
                children_ =
                    List.reverse acc.done

                ( state_, label_ ) =
                    f state { children = children_, node = acc.label, ancestors = acc.parents }

                node =
                    Tree label_ children_
            in
            case stack of
                [] ->
                    ( state_, node )

                top :: rest ->
                    mapAccumulateWithContextBottomUpHelp f state_ { top | done = node :: top.done } rest

        (Tree d []) :: rest ->
            let
                ( state_, label_ ) =
                    f state { children = [], node = d, ancestors = acc.parents }
            in
            mapAccumulateWithContextBottomUpHelp f
                state_
                { acc
                    | todo = rest
                    , done = Tree label_ [] :: acc.done
                }
                stack

        (Tree d cs) :: rest ->
            mapAccumulateWithContextBottomUpHelp f
                state
                { todo = cs
                , done = []
                , label = d
                , parents = d :: acc.parents
                }
                ({ acc | todo = rest } :: stack)


type alias MapAccCtxBottomUp a b =
    { todo : List (Tree a)
    , done : List (Tree b)
    , parents : List a
    , label : a
    }


{-| Sorts all children of each node based on the comparator function (the function recieves a list of ancestors).

    tree 1
        [ tree 3
            [ singleton 5
            , singleton 4
            ]
        , singleton 2
        , singleton 6
        ]
        |> sortWith (\_ a b -> compare (label a) (label b))
    --> tree 1
    -->   [ singleton 2
    -->    , tree 3
    -->        [ singleton 4
    -->        , singleton 5
    -->        ]
    -->    , singleton 6
    -->    ]

-}
sortWith : (List a -> Tree a -> Tree a -> Order) -> Tree a -> Tree a
sortWith compareFn (Tree d cs) =
    sortWithHelp compareFn
        { todo = cs
        , done = []
        , label = d
        , parents = [ d ]
        }
        []


sortWithHelp : (List a -> Tree a -> Tree a -> Order) -> MapAccCtxBottomUp a a -> List (MapAccCtxBottomUp a a) -> Tree a
sortWithHelp compareFn acc stack =
    case acc.todo of
        [] ->
            let
                children_ =
                    List.sortWith (compareFn acc.parents) acc.done

                node =
                    Tree acc.label children_
            in
            case stack of
                [] ->
                    node

                top :: rest ->
                    sortWithHelp compareFn { top | done = node :: top.done } rest

        (Tree d []) :: rest ->
            sortWithHelp compareFn
                { acc
                    | todo = rest
                    , done = Tree d [] :: acc.done
                }
                stack

        (Tree d cs) :: rest ->
            sortWithHelp compareFn
                { todo = cs
                , done = []
                , label = d
                , parents = d :: acc.parents
                }
                ({ acc | todo = rest } :: stack)


{-| Finds a subtree whose label matches the predicate.

Searches the tree in a breadth-first manner.

    tree 1
        [ tree 3
            [ singleton 5
            , singleton 4
            ]
        , singleton 2
        , singleton 6
        ]
        |> findBfs (\a -> label a == 3)
    --> Just (tree 3 [ singleton 5, singleton 4 ])

-}
findBfs : (Tree a -> Bool) -> Tree a -> Maybe (Tree a)
findBfs predicate t =
    findBfsHelp predicate (Fifo [ t ] [])


findBfsHelp : (Tree a -> Bool) -> Queue (Tree a) -> Maybe (Tree a)
findBfsHelp predicate queue =
    case removeQueue queue of
        ( Just t, rest ) ->
            if predicate t then
                Just t

            else
                findBfsHelp predicate (insertQueue t rest)

        ( Nothing, _ ) ->
            Nothing


type Queue a
    = Fifo (List a) (List a)


insertQueue : Tree a -> Queue (Tree a) -> Queue (Tree a)
insertQueue (Tree _ cs) (Fifo front back) =
    Fifo front (List.reverse cs ++ back)


removeQueue : Queue a -> ( Maybe a, Queue a )
removeQueue fifo =
    case fifo of
        Fifo [] [] ->
            ( Nothing, fifo )

        Fifo [] back ->
            removeQueue <| Fifo (List.reverse back) []

        Fifo (next :: rest) back ->
            ( Just next, Fifo rest back )


{-| Restructure a `Tree` into another type of structure.

Imagine you have a `Tree String` and you can to turn it into nested `<ul>`s.
This function can help!

    import Html exposing (Html)


    labelToHtml : String -> Html msg
    labelToHtml l =
        Html.text l


    toListItems : Html msg -> List (Html msg) -> Html msg
    toListItems label children =
        case children of
            [] ->
                Html.li [] [ label ]
            _ ->
                Html.li []
                    [ label
                    , Html.ul [] children
                    ]


    tree "root"
        [ tree "folder"
            [ singleton "foo"
            , singleton "bar"
            ]
        , singleton "yeah"
        ]
        |> restructure labelToHtml toListItems
        |> \root -> Html.ul [] [ root ]
    --> Html.ul []
    -->     [ Html.li []
    -->         [ Html.text "root"
    -->         , Html.ul []
    -->             [ Html.li []
    -->                 [ Html.text "folder"
    -->                 , Html.ul []
    -->                     [ Html.li [] [ Html.text "foo" ]
    -->                     , Html.li [] [ Html.text "bar" ]
    -->                     ]
    -->                 ]
    -->             , Html.li [] [ Html.text "yeah" ]
    -->             ]
    -->         ]
    -->     ]

Or perhaps you have your own tree datastructure and you want to convert to it:

    type MyTree a = MyTree a (List (MyTree a))


    tree "root"
        [ tree "folder"
            [ singleton "foo"
            , singleton "bar"
            ]
        , singleton "yeah"
        ]
        |> restructure identity MyTree
    --> MyTree "root"
    -->     [ MyTree "folder"
    -->         [ MyTree "foo" []
    -->         , MyTree "bar" []
    -->         ]
    -->     , MyTree "yeah" []
    -->     ]

-}
restructure : (a -> b) -> (b -> List c -> c) -> Tree a -> c
restructure convertLabel convertTree (Tree l c) =
    restructureHelp convertLabel
        convertTree
        { todo = c
        , label = convertLabel l
        , done = []
        }
        []


restructureHelp :
    (a -> b)
    -> (b -> List c -> c)
    -> ReAcc a b c
    -> List (ReAcc a b c)
    -> c
restructureHelp fLabel fTree acc stack =
    case acc.todo of
        [] ->
            let
                node =
                    fTree acc.label (List.reverse acc.done)
            in
            case stack of
                [] ->
                    node

                top :: rest ->
                    restructureHelp
                        fLabel
                        fTree
                        { top | done = node :: top.done }
                        rest

        (Tree l []) :: rest ->
            restructureHelp
                fLabel
                fTree
                { acc
                    | todo = rest
                    , done = fTree (fLabel l) [] :: acc.done
                }
                stack

        (Tree l cs) :: rest ->
            restructureHelp
                fLabel
                fTree
                { todo = cs
                , done = []
                , label = fLabel l
                }
                ({ acc | todo = rest } :: stack)


type alias ReAcc a b c =
    { todo : List (Tree a)
    , done : List c
    , label : b
    }
