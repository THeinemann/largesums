module Modules.LargeSums exposing (..)

import PracticeModule exposing (Answer(..), Msg(..), PracticeModule)
import Random



-- MODEL


type alias Task =
    { sum : Int
    , summands : List Int
    }


fromInt : Int -> Task
fromInt x =
    let
        loop exp r acc =
            if r == 0 then
                { sum = x, summands = List.filter (\s -> s /= 0) acc }

            else
                loop (exp + 1) (r // 10) (modBy 10 r * (10 ^ exp) :: acc)
    in
    loop 0 x []


sumString : Task -> String
sumString task =
    String.concat (List.intersperse " + " (List.map String.fromInt task.summands))


type alias Answer =
    PracticeModule.Answer Task


type alias GameState =
    PracticeModule.GameState Task


buildTasks : Cmd Msg
buildTasks =
    Random.generate (\list -> Input (List.map fromInt list)) (Random.list 20 (Random.int 1000 1000000))


type alias Msg =
    PracticeModule.Msg Task



-- MODULE


largeSums : PracticeModule Task
largeSums =
    { name = "Große Summen"
    , init = \_ -> ( { currentValue = Nothing, remaining = [], answered = [], previous = Nothing }, buildTasks )
    , defaultTask = fromInt 0
    , question = \task -> "Was ist " ++ sumString task ++ "?"
    , expected = \task -> task.sum
    , errorMessage = \task -> sumString task ++ " ist " ++ String.fromInt task.sum ++ "."
    }
