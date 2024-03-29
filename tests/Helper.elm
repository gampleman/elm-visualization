module Helper exposing (assert, atLeastFloat, atMostFloat, expectAll, expectAny, expectMember, isBetween, pathEqual)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Path exposing (Path)
import Regex
import Test.Runner


numRegex : Regex.Regex
numRegex =
    Regex.fromString "[-+]?(?:\\d+\\.\\d+|\\d+\\.|\\.\\d+|\\d+)(?:[eE][-]?\\d+)?"
        |> Maybe.withDefault Regex.never


pathEqual : String -> Path -> Expect.Expectation
pathEqual str path =
    let
        format s =
            if abs (s - toFloat (round s)) < 1.0e-6 then
                String.fromInt (round s)

            else
                String.fromFloat (toFloat (round (s * 1.0e6)) / 1.0e6)

        normalize =
            Regex.replace numRegex (\{ match } -> format <| Maybe.withDefault 0 <| String.toFloat match)
                >> Path.parse

        pathStr =
            normalize (Path.toString path)

        normStr =
            normalize str
    in
    case ( normStr, pathStr ) of
        ( Ok normParse, Ok pathParse ) ->
            Expect.equal (Path.toString normParse) (Path.toString pathParse)

        ( Err a, Err b ) ->
            Expect.fail ("Parsing both strings failed with:" ++ Debug.toString ( a, b ))

        ( _, Err e ) ->
            Expect.fail ("Parsing the expected failed with:" ++ Debug.toString e)

        ( Err e, _ ) ->
            Expect.fail ("Parsing the model failed with:" ++ Debug.toString e)


precision : number
precision =
    100000


isBetween : ( Float, Float ) -> Float -> Expectation
isBetween ( b, c ) a =
    let
        mi =
            min b c

        ma =
            max b c
    in
    if a >= mi && a <= ma then
        Expect.pass

    else
        let
            withinExpAMin =
                Expect.within (Absolute (1 / precision)) a mi

            withinExpAMax =
                Expect.within (Absolute (1 / precision)) a ma
        in
        if withinExpAMin == Expect.pass || withinExpAMax == Expect.pass then
            Expect.pass

        else
            Expect.fail (Debug.toString a ++ "\n╷\n| isBetween\n╵\n" ++ Debug.toString ( mi, ma ))


atMostFloat : Float -> Float -> Expectation
atMostFloat a b =
    compareOrEqual (<=) b a "atMost"


atLeastFloat : Float -> Float -> Expectation
atLeastFloat a b =
    compareOrEqual (>=) b a "atLeast"


compareOrEqual : (Float -> Float -> Bool) -> Float -> Float -> String -> Expectation
compareOrEqual compareFun a b compStr =
    if compareFun a b then
        Expect.pass

    else
        let
            withinExp =
                Expect.within (Absolute (1 / precision)) a b
        in
        if withinExp == Expect.pass then
            Expect.pass

        else
            Expect.fail (Debug.toString a ++ "\n╷\n|" ++ compStr ++ "\n╵\n" ++ Debug.toString b)


expectAll : List Expectation -> Expectation
expectAll expectations =
    Expect.all (List.map always expectations) ()


expectAny : List Expectation -> Expectation
expectAny expectations =
    let
        failures =
            List.filterMap Test.Runner.getFailureReason expectations
    in
    if List.length failures == List.length expectations then
        Expect.fail <| "Expected at least one of the following to pass:\n" ++ (String.join "\n" <| List.map (.reason >> Debug.toString) failures)

    else
        Expect.pass


expectMember : List a -> a -> Expectation
expectMember list item =
    if List.member item list then
        Expect.pass

    else
        Expect.fail ("Expected " ++ Debug.toString item ++ " to be a member of " ++ Debug.toString list)


assert : String -> Bool -> Expectation
assert str bool =
    if bool then
        Expect.pass

    else
        Expect.fail str
