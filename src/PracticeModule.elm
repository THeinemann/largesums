module PracticeModule exposing (..)

import Bootstrap.Alert exposing (simpleDanger, simpleSuccess)
import Html
import Html.Styled exposing (Html, fromUnstyled, h2, text)
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


initialised : GameState task -> Bool
initialised gamestate =
    not (List.isEmpty gamestate.remaining && List.isEmpty gamestate.answered)


type Msg task
    = Change String
    | Submit
    | Input (List task)
    | Reset


type alias PracticeModule task =
    { name : String
    , init : () -> ( GameState task, Cmd (Msg task) )
    , viewContents : GameState task -> Html (Msg task)
    , defaultTask : task
    , expected : task -> Int
    , errorMessage : task -> String
    }



-- UPDATE


update : PracticeModule task -> Msg task -> GameState task -> ( GameState task, Cmd (Msg task) )
update mod msg gameState =
    case msg of
        Reset ->
            mod.init ()

        Change val ->
            ( { gameState | currentValue = String.toInt val }, Cmd.none )

        Submit ->
            let
                currentTask =
                    List.head gameState.remaining |> Maybe.withDefault mod.defaultTask

                expected =
                    mod.expected currentTask

                answer =
                    if Maybe.withDefault -1 gameState.currentValue == expected then
                        Correct

                    else
                        Wrong currentTask

                updatedAnswers =
                    answer :: gameState.answered

                newModel =
                    { currentValue = Nothing
                    , remaining = List.tail gameState.remaining |> Maybe.withDefault []
                    , answered = updatedAnswers
                    , previous = Just answer
                    }
            in
            ( newModel, Cmd.none )

        Input list ->
            ( { gameState | remaining = list }, Cmd.none )



-- VIEW


answerMessage : PracticeModule task -> Answer task -> Html msg
answerMessage mod answer =
    let
        unstyled =
            case answer of
                Correct ->
                    simpleSuccess [] [ Html.text "Richtig!" ]

                Wrong task ->
                    simpleDanger [] [ Html.text ("Leider Falsch. " ++ mod.errorMessage task) ]
    in
    fromUnstyled unstyled


view : PracticeModule task -> GameState task -> Html (Msg task)
view mod model =
    let
        contents =
            if not (initialised model) then
                h2 [] [ text "Laden. Bitte warten..." ]

            else
                mod.viewContents model
    in
    mainWindow mod.name contents
