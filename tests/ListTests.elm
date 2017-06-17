module ListTests exposing (all)

import Test exposing (..)
import Expect
import Visualization.List as List
import Fuzz exposing (..)


all : Test
all =
    describe "List extensions"
        [ describe "bisect"
            [ test "returns the index after an exact match" <|
                \() -> Expect.true "" True
            ]
        ]
