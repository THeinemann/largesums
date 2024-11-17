module LargeSums.LargeSums exposing (..)

import PracticeModule exposing (Answer(..), Msg(..), PracticeModule)
import Random
import SumTask exposing (Task, fromInt)


type alias Answer =
    PracticeModule.Answer Task


type alias GameState =
    PracticeModule.GameState Task


buildTasks : Cmd Msg
buildTasks =
    Random.generate (\list -> Input (List.map fromInt list)) (Random.list 20 (Random.int 1000 1000000))


type alias Msg =
    PracticeModule.Msg Task


largeSums : PracticeModule Task
largeSums =
    { name = "Große Summen"
    , init = \_ -> ( { currentValue = Nothing, remaining = [], answered = [], previous = Nothing }, buildTasks )
    , defaultTask = SumTask.fromInt 0
    , question = \task -> "Was ist " ++ SumTask.sumString task ++ "?"
    , expected = \task -> task.sum
    , errorMessage = \task -> SumTask.sumString task ++ " ist " ++ String.fromInt task.sum ++ "."
    }
