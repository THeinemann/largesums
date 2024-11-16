module MainTest exposing (..)

import Expect
import LargeSums.LargeSums as LargeSums
import Test exposing (..)


expectFalse : Bool -> Expect.Expectation
expectFalse =
    Expect.equal False


initTests : Test
initTests =
    describe "Initialisation" <|
        let
            ( actualMessage, actualCommand ) =
                LargeSums.init ()
        in
        [ test "Should start with correct initial command"
            (\_ -> Expect.equal actualCommand LargeSums.buildTasks)
        , test "Should start with correct initial state" <|
            \_ ->
                LargeSums.initialised actualMessage
                    |> expectFalse
        ]
