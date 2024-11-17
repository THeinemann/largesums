module Modules.Doubling exposing (..)

import PracticeModule exposing (Answer(..), Msg(..), PracticeModule)
import Random
import Random.List


type alias Answer =
    PracticeModule.Answer Int


type alias GameState =
    PracticeModule.GameState Int


shuffleCommand : Cmd Msg
shuffleCommand =
    Random.generate Input (Random.List.shuffle (List.range 1 20))


type alias Msg =
    PracticeModule.Msg Int


doubling : PracticeModule Int
doubling =
    { name = "Verdoppeln"
    , init = \_ -> ( { currentValue = Nothing, remaining = [], answered = [], previous = Nothing }, shuffleCommand )
    , defaultTask = 0
    , question = \n -> "Was ist das Doppelte von " ++ String.fromInt n ++ "?"
    , expected = \n -> n * 2
    , errorMessage = \n -> "Das Doppelte von " ++ String.fromInt n ++ " ist " ++ String.fromInt (n * 2) ++ "."
    }
