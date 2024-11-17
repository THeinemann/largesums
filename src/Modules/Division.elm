module Modules.Division exposing (..)

import PracticeModule exposing (Answer(..), Msg(..), PracticeModule)
import Random
import Random.List


type alias DivisionTask =
    { dividend : Int
    , divisor : Int
    }


fromFactors : Int -> Int -> DivisionTask
fromFactors x y =
    { dividend = x * y, divisor = y }


type alias Answer =
    PracticeModule.Answer DivisionTask


type alias GameState =
    PracticeModule.GameState DivisionTask


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f tuple =
    case tuple of
        ( a, b ) ->
            f a b


buildTasksCommmand : Cmd Msg
buildTasksCommmand =
    Random.generate (\list -> Input (List.map (uncurry fromFactors) list)) (Random.list 20 (Random.pair (Random.int 1 10) (Random.int 1 10)))


type alias Msg =
    PracticeModule.Msg DivisionTask


division : PracticeModule DivisionTask
division =
    { name = "Division"
    , init = \_ -> ( { currentValue = Nothing, remaining = [], answered = [], previous = Nothing }, buildTasksCommmand )
    , defaultTask = fromFactors 0 1
    , question = \task -> "Was ist " ++ String.fromInt task.dividend ++ " / " ++ String.fromInt task.divisor ++ "?"
    , expected = \task -> task.dividend // task.divisor
    , errorMessage = \task -> String.fromInt task.dividend ++ " / " ++ String.fromInt task.divisor ++ " ist " ++ String.fromInt (task.dividend // task.divisor)
    }
