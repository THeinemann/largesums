module MainTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Main
import Tuple exposing (first)
import Random
import Random.List


suite : Test
suite =
    test "Should start with correct initial state" (
        let expectedMessage = Main.Initialising
            expctedCommand = Main.shuffleCommand
         in (\_ -> Expect.equal (Main.init ()) (expectedMessage, expctedCommand))
    )