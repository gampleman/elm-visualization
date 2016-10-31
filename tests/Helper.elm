module Helper exposing (..)

import Expect exposing (Expectation)
import Regex exposing (HowMany(..))
import String
import Result
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
    if round (a * precision) == round (b * precision) then
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
    case expectations of
        [] ->
            Expect.pass

        x :: xs ->
            case Expect.getFailure x of
                Nothing ->
                    expectAll xs

                Just _ ->
                    x
