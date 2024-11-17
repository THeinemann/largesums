module PracticeModule exposing (..)

import Html.Styled exposing (Html, h2, text)
import Styling exposing (mainWindow)



-- MODEL


type Answer task
    = Correct
    | Wrong task


isCorrect : Answer task -> Bool
isCorrect answer =
    case answer of
        Correct ->
            True

        _ ->
            False


type alias GameState task =
    { currentValue : Maybe Int
    , remaining : List task
    , answered : List (Answer task)
    , previous : Maybe (Answer task)
    }


type Msg task
    = Change String
    | Submit
    | Input (List task)
    | Reset


type alias PracticeModule task =
    { name : String
    , init : () -> ( GameState task, Cmd (Msg task) )
    , initialised : GameState task -> Bool
    , update : Msg task -> GameState task -> ( GameState task, Cmd (Msg task) )
    , viewContents : GameState task -> Html (Msg task)
    }


view : PracticeModule task -> GameState task -> Html (Msg task)
view mod model =
    let
        contents =
            if not (mod.initialised model) then
                h2 [] [ text "Laden. Bitte warten..." ]

            else
                mod.viewContents model
    in
    mainWindow mod.name contents
