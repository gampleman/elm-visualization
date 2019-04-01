module Helper exposing (expectAll, expectAny, expectMember, isAbout, isBetween, pathEqual, precision)

import Expect exposing (Expectation)
import Path exposing (Path)
import Regex
import Result
import Test.Runner exposing (getFailureReason)
import Test.Runner.Failure


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


precision =
    100000


isAbout a b =
    if truncate ((a - b) * precision) == 0 then
        Expect.pass

    else
        Expect.equal a b


isBetween ( b, c ) a =
    let
        mi =
            min b c

        ma =
            max b c
    in
    if round (a * precision) >= round (mi * precision) && round (a * precision) <= round (ma * precision) then
        Expect.pass

    else
        Expect.fail (Debug.toString a ++ "\n╷\n| isBetween\n╵\n" ++ Debug.toString ( mi, ma ))


expectAll : List Expectation -> Expectation
expectAll expectations =
    Expect.all (List.map always expectations) ()


expectAny : List Expectation -> Expectation
expectAny expectations =
    let
        failuires =
            List.filterMap Test.Runner.getFailureReason expectations
    in
    if List.length failuires == List.length expectations then
        Expect.fail <| (++) "Expected at least one of the following to pass:\n" <| String.join "\n" <| List.map (.reason >> Test.Runner.Failure.format "") failuires

    else
        Expect.pass


expectMember : List a -> a -> Expectation
expectMember list item =
    Expect.true "expectMember" <| List.member item list
