module MainTest exposing (..)

import Expect
import Modules.LargeSums as LargeSums exposing (largeSums)
import PracticeModule
import Test exposing (..)


expectFalse : Bool -> Expect.Expectation
expectFalse =
    Expect.equal False


initTests : Test
initTests =
    describe "Initialisation" <|
        let
            ( actualMessage, actualCommand ) =
                largeSums.init ()
        in
        [ test "Should start with correct initial command"
            (\_ -> Expect.equal actualCommand LargeSums.buildTasks)
        , test "Should start with correct initial state" <|
            \_ ->
                PracticeModule.initialised actualMessage
                    |> expectFalse
        ]
