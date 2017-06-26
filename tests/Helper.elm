module Helper exposing (..)

import Expect exposing (Expectation)
import Regex exposing (HowMany(..))
import Result
import Test.Runner exposing (getFailure)
import Visualization.Path as Path exposing (..)


pathEqual : String -> String -> Expect.Expectation
pathEqual str path =
    let
        format s =
            if abs (s - (toFloat (round s))) < 1.0e-6 then
                toString (round s)
            else
                toString (toFloat (round (s * 1.0e6)) / 1.0e6)

        normalize =
            Regex.replace All (Regex.regex "[-+]?(?:\\d+\\.\\d+|\\d+\\.|\\.\\d+|\\d+)(?:[eE][-]?\\d+)?") (\{ match } -> format <| Result.withDefault 0 <| String.toFloat match)

        pathStr =
            normalize path

        normStr =
            normalize str
    in
        Expect.equal normStr pathStr


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
            Expect.fail (toString a ++ "\n╷\n| isBetween\n╵\n" ++ toString ( mi, ma ))


expectAll : List Expectation -> Expectation
expectAll expectations =
    Expect.all (List.map always expectations) ()


expectAny : List Expectation -> Expectation
expectAny expectations =
    let
        failuires =
            List.filterMap Test.Runner.getFailure expectations
    in
        if List.length failuires == List.length expectations then
            Expect.fail <| (++) "Expected at least one of the following to pass:\n" <| String.join "\n" <| List.map .message failuires
        else
            Expect.pass


expectMember : List a -> a -> Expectation
expectMember list item =
    Expect.true "expectMember" <| List.member item list
