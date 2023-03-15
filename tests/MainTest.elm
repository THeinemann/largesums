module MainTest exposing (..)

import Expect
import Test exposing (..)
import Main


suite : Test
suite =
    test "Should start with correct initial state" (
        let expectedMessage = Main.Initialising
            expctedCommand = Main.shuffleCommand
         in (\_ -> Expect.equal (Main.init ()) (expectedMessage, expctedCommand))
    )