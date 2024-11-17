module LargeSums.LargeSums exposing (..)

import Bootstrap.Alert exposing (simpleDanger, simpleSuccess)
import Html
import Html.Styled exposing (Html, br, button, div, fromUnstyled, h2, input, text)
import Html.Styled.Attributes as A exposing (autofocus, class, css, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Html.Styled.Keyed as Keyed
import PracticeModule exposing (Answer(..), Msg(..), PracticeModule, isCorrect)
import Random
import Styling exposing (defaultMargin)
import SumTask exposing (Task, fromInt)



-- MODEL


type alias Answer =
    PracticeModule.Answer Task


type alias GameState =
    PracticeModule.GameState Task


buildTasks : Cmd Msg
buildTasks =
    Random.generate (\list -> Input (List.map fromInt list)) (Random.list 20 (Random.int 1000 1000000))



-- UPDATE


type alias Msg =
    PracticeModule.Msg Task



-- VIEW


answerMessage : Answer -> Html msg
answerMessage answer =
    let
        unstyled =
            case answer of
                Correct ->
                    simpleSuccess [] [ Html.text "Richtig!" ]

                Wrong task ->
                    simpleDanger [] [ Html.text ("Leider Falsch. " ++ SumTask.sumString task ++ " ist " ++ String.fromInt task.sum ++ ".") ]
    in
    fromUnstyled unstyled


viewContents : GameState -> Html Msg
viewContents model =
    if not (PracticeModule.initialised model) then
        h2 [] [ text "Laden. Bitte warten..." ]

    else
        let
            feedback =
                Maybe.map (\answer -> [ answerMessage answer ]) model.previous |> Maybe.withDefault []
        in
        case model.remaining of
            currentTask :: _ ->
                div []
                    [ Keyed.node "form"
                        [ onSubmit Submit ]
                        (List.map (\x -> ( "feedback", x )) feedback
                            ++ [ ( "question", div [] [ text ("Was ist " ++ SumTask.sumString currentTask ++ "?") ] )
                               , ( "input"
                                 , input
                                    [ onInput Change
                                    , type_ "number"
                                    , value (Maybe.map String.fromInt model.currentValue |> Maybe.withDefault "")
                                    , css defaultMargin
                                    , autofocus True
                                    , A.required True
                                    ]
                                    []
                                 )
                               , ( "br", br [] [] )
                               , ( "submitButton", input [ type_ "submit", css defaultMargin, value "Ok", class "btn btn-primary" ] [] )
                               ]
                        )
                    ]

            [] ->
                let
                    numbers =
                        List.length model.answered

                    correctAnswers =
                        List.length (List.filter isCorrect model.answered)
                in
                div []
                    (feedback
                        ++ [ div [] [ text "Game over.\n" ]
                           , div [] [ text ("Du hast " ++ String.fromInt correctAnswers ++ " von " ++ String.fromInt numbers ++ " Aufgaben korrekt gelöst!") ]
                           , button [ onClick Reset, css defaultMargin, class "btn btn-primary" ] [ text "Nochmal!" ]
                           ]
                    )


largeSums : PracticeModule Task
largeSums =
    { name = "Große Summen"
    , init = \_ -> ( { currentValue = Nothing, remaining = [], answered = [], previous = Nothing }, buildTasks )
    , viewContents = viewContents
    , defaultTask = SumTask.fromInt 0
    , expected = \task -> task.sum
    }
