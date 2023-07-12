module Hierarchy exposing (..)

import Dict exposing (Dict)
import Hierarchy.Tidy
import Hierarchy.Tree as Tree exposing (Tree)
import Hierarchy.Treemap
import Set exposing (Set)



-- TODO: Tail recursive


type StratifyError
    = MultipleRoots
    | NoRoot


stratifyWithPath : { path : a -> ( comparable, List comparable ), createMissingNode : ( comparable, List comparable ) -> a } -> List a -> Result StratifyError (Tree a)
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


stratify : { id : a -> comparable, parentId : a -> Maybe comparable, transform : a -> b } -> List a -> Result StratifyError (Tree b)
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
    Tree.tree item (Dict.get itemId parentBag |> Maybe.withDefault [] |> List.map (stratifyHelp parentBag))



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
