module MyMarkdown exposing (strip, render)

import Html exposing (Html)
import Html.Attributes as Attr
import Markdown.Block as Block exposing (..)
import Markdown.Renderer exposing (Renderer)
import Markdown.Parser
import Markdown.Html


render : String -> Html.Html msg 
render  =
    Markdown.Parser.parse 
    >> Result.withDefault []
    >> Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer
    >> Result.withDefault ([Html.text "Something went wrong"])
    >> Html.div []



strip : String -> String
strip =
    Markdown.Parser.parse 
    >> Result.withDefault []
    >> Markdown.Renderer.render renderer
    >> Result.withDefault []
    >> String.join " "
    >> String.replace "  " " "

renderer : Renderer String
renderer =
    { heading = \{ children } -> String.concat children
    , paragraph =  String.concat
    , hardLineBreak = "\n"
    , blockQuote =  String.concat
    , strong =  String.concat
    , emphasis =  String.concat
    , codeSpan =  identity
    , link = \link content ->  String.concat content
    , image = \imageInfo -> ""
    , text = identity
    , unorderedList =
        \items ->
            items
                    |> List.map
                        (\(Block.ListItem _ children) ->
                            String.concat children
                        )
                    |> String.join " "
                
    , orderedList =
        \startingIndex items ->
            
                (items
                    |> List.indexedMap
                        (\ idx itemBlocks ->
                            String.fromInt (idx + startingIndex) ++ ") " ++ String.concat itemBlocks
                        )
                    |> String.join " "
                )
    , html = Markdown.Html.oneOf [] |> Markdown.Html.map (always String.concat)
    , codeBlock = \block -> block.body
                   
    , thematicBreak = " "
    , table = always ""
    , tableHeader = always ""
    , tableBody = always ""
    , tableRow =always ""
    , tableHeaderCell = \_ _ -> ""
    , tableCell = \_ _ -> ""
    , strikethrough = String.concat
    }
