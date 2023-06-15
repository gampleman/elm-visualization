module Hierarchy exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)


type Hierarchy a
    = Hierarchy a (List (Hierarchy a))


hierarchy : (data -> a) -> (data -> List data) -> data -> Hierarchy a
hierarchy accessor children data =
    Hierarchy (accessor data) (List.map (hierarchy accessor children) (children data))



-- TODO: Tail recursive


node : Hierarchy a -> a
node (Hierarchy a _) =
    a


toList (Hierarchy n children) =
    n :: List.concatMap toList children


leaves : Hierarchy a -> List a
leaves (Hierarchy n children) =
    case children of
        [] ->
            [ n ]

        _ ->
            List.concatMap leaves children


map : (a -> b) -> Hierarchy a -> Hierarchy b
map fn (Hierarchy val children) =
    Hierarchy (fn val) (List.map (map fn) children)


mapWithContextTopDown : ({ ancestors : List b, node : a, children : List (Hierarchy a) } -> b) -> Hierarchy a -> Hierarchy b
mapWithContextTopDown fn tree =
    let
        help parents (Hierarchy val children) =
            let
                processed =
                    fn { ancestors = parents, children = children, node = val }
            in
            Hierarchy processed (List.map (help (processed :: parents)) children)
    in
    help [] tree


mapWithContextBottomUp : ({ ancestors : List a, node : a, children : List (Hierarchy b) } -> b) -> Hierarchy a -> Hierarchy b
mapWithContextBottomUp fn tree =
    let
        help parents (Hierarchy val children) =
            let
                processed =
                    List.map (help (val :: parents)) children
            in
            Hierarchy (fn { ancestors = parents, node = val, children = processed }) processed
    in
    help [] tree


type StratifyError
    = MultipleRoots
    | NoRoot


stratifyWithPath : { path : a -> ( comparable, List comparable ), createMissingNode : ( comparable, List comparable ) -> a } -> List a -> Result StratifyError (Hierarchy a)
stratifyWithPath { path, createMissingNode } nodes =
    List.map (\item -> ( path item, item )) nodes
        |> List.sortBy (Tuple.first >> Tuple.second >> List.length)
        |> List.foldl
            (\( ( root, rest ), item ) ( seenSoFar, nodes_ ) ->
                let
                    wrapNode id parentId n =
                        { id = id, parentId = parentId, node = n }

                    createParentNodes revPath newNodes =
                        case revPath of
                            [] ->
                                newNodes

                            x :: xs ->
                                if Set.member x seenSoFar then
                                    createParentNodes xs newNodes

                                else
                                    createParentNodes xs (wrapNode x (List.head xs) (createMissingNode ( root, List.tail (List.reverse revPath) |> Maybe.withDefault [] )) :: newNodes)

                    reversedPath =
                        (root :: rest) |> List.reverse

                    current =
                        wrapNode (reversedPath |> List.head |> Maybe.withDefault root) (reversedPath |> List.tail |> Maybe.andThen List.head) item

                    nodesToCreate =
                        createParentNodes (reversedPath |> List.tail |> Maybe.withDefault []) [ current ]
                in
                ( List.foldl (\i -> Set.insert i.id) seenSoFar nodesToCreate, nodesToCreate :: nodes_ )
            )
            ( Set.empty, [] )
        |> Tuple.second
        |> List.concat
        |> stratify { id = .id, parentId = .parentId, transform = .node }


stratify : { id : a -> comparable, parentId : a -> Maybe comparable, transform : a -> b } -> List a -> Result StratifyError (Hierarchy b)
stratify { id, parentId, transform } nodes =
    List.foldl
        (\item ->
            Result.andThen
                (\( maybeRoot, parentBag ) ->
                    let
                        nodeId =
                            id item

                        node_ =
                            transform item

                        nodeParentId =
                            parentId item
                    in
                    case nodeParentId of
                        Nothing ->
                            if maybeRoot == Nothing then
                                Ok ( Just ( nodeId, node_ ), parentBag )

                            else
                                Err MultipleRoots

                        Just actualNodeParentId ->
                            Ok ( maybeRoot, Dict.update actualNodeParentId (\items -> Just (( nodeId, node_ ) :: Maybe.withDefault [] items)) parentBag )
                )
        )
        (Ok ( Nothing, Dict.empty ))
        nodes
        |> Result.andThen
            (\( maybeRoot, parentBag ) ->
                case maybeRoot of
                    Just root ->
                        Ok (stratifyHelp parentBag root)

                    Nothing ->
                        Err NoRoot
            )



-- TODO: detect cycles


stratifyHelp parentBag ( itemId, item ) =
    Hierarchy item (Dict.get itemId parentBag |> Maybe.withDefault [] |> List.map (stratifyHelp parentBag))



{-

   sum : (a -> Float) -> (a -> Float -> a) -> Tree a -> Tree a
   sum get set =
       Tree.restructure identity
           (\val children ->
               let
                   total =
                       children |> List.map (Tree.children >> get) |> List.sum
               in
               set val (get val + total)
           )


   sortWith : (Tree a -> Tree a -> Order) -> Tree a -> Tree a
   sortWith sortFn =
       Tree.mapChildren
           (List.sortWith sortFn
               >> List.map (sortWith sortFn)
           )



   --- TIDY TREE


   tidy : (Tree.Zipper a -> Tree.Zipper a -> Float) -> TidySize -> Tree a -> Tree ( ( Float, Float ), a )
   tidy seperation size inputTree =
       Tree.indexedMap
           (\i n ->
               { prelim = 0
               , mod = 0
               , change = 0
               , shift = 0
               , thread = Nothing
               , i = i
               , value = ( ( 0, 0 ), n )
               }
           )
           inputTree
-}
