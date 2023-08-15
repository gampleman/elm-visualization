module Events exposing (Rect, Touch, decodeMousePosition, decodeSVGTransformMatrix, decodeTouches, normalizePointerPosition)

import Json.Decode as D exposing (Decoder)
import Zoom.Matrix as Matrix exposing (Matrix2x3)


type alias Touch =
    { position : ( Float, Float )
    , identifier : Int
    }


type alias Rect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


normalizePointerPosition : ( Float, Float ) -> Maybe Matrix2x3 -> ( Float, Float )
normalizePointerPosition position maybeMatrix =
    case maybeMatrix of
        Just matrix ->
            Matrix.transform position matrix

        Nothing ->
            position


decodeMousePosition : Decoder ( Float, Float )
decodeMousePosition =
    D.map3
        (\maybeMatrix x y ->
            normalizePointerPosition ( x, y ) maybeMatrix
        )
        decodeSVGTransformMatrix
        (D.oneOf [ D.field "offsetX" D.float, D.field "clientX" D.float ])
        (D.oneOf [ D.field "offsetY" D.float, D.field "clientY" D.float ])


decodeSVGTransformMatrix : Decoder (Maybe Matrix2x3)
decodeSVGTransformMatrix =
    D.oneOf
        [ D.map3
            (\viewBox width height ->
                Just ( ( viewBox.width / width, 0, 0 ), ( 0, viewBox.height / height, 0 ) )
            )
            (D.at [ "currentTarget", "viewBox", "baseVal" ] decodeRect)
            (D.at [ "currentTarget", "width", "baseVal", "value" ] D.float)
            (D.at [ "currentTarget", "height", "baseVal", "value" ] D.float)
        , D.succeed Nothing
        ]



{- FFS we need this shenigan to decode these bizzaro datastructures -}


listLike : Decoder a -> Decoder (List a)
listLike itemDecoder =
    let
        decodeN n =
            List.range 0 (n - 1)
                |> List.map decodeOne
                |> List.foldr (D.map2 (::)) (D.succeed [])

        decodeOne n =
            D.field (String.fromInt n) itemDecoder
    in
    D.field "length" D.int
        |> D.andThen decodeN


decodeTouches : Decoder (List Touch)
decodeTouches =
    D.andThen
        (\maybeMatrix ->
            D.map3
                (\x y identifier ->
                    { position = normalizePointerPosition ( x, y ) maybeMatrix, identifier = identifier }
                )
                (D.field "clientX" D.float)
                (D.field "clientY" D.float)
                (D.field "identifier" D.int)
                |> listLike
                |> D.field "changedTouches"
        )
        decodeSVGTransformMatrix


decodeRect : Decoder Rect
decodeRect =
    D.map4 Rect
        (D.field "x" D.float)
        (D.field "y" D.float)
        (D.field "width" D.float)
        (D.field "height" D.float)
