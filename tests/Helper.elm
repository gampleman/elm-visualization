module Helper exposing (..)

import Expect exposing (Expectation)


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
