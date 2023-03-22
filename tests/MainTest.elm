module MainTest exposing (..)

import Expect
import Test exposing (..)
import Main


expectFalse : Bool -> Expect.Expectation
expectFalse = Expect.equal False

initTests : Test
initTests =
    describe "Initialisation" <|
        let (actualMessage, actualCommand) = Main.init()
         in [ test "Should start with correct initial command" (
                    (\_ -> Expect.equal (actualCommand) Main.shuffleCommand)
                )
            , test "Should start with correct initial state" <|
                \_ ->
                    Main.initialised actualMessage
                        |> expectFalse
            ]