module Doubling.Doubling exposing (..)

import Bootstrap.Alert exposing (simpleDanger, simpleSuccess)
import Html
import Html.Styled exposing (Html, br, button, div, fromUnstyled, h2, input, text)
import Html.Styled.Attributes as A exposing (autofocus, class, css, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Html.Styled.Keyed as Keyed
import PracticeModule exposing (Answer(..), Msg(..), PracticeModule, isCorrect)
import Random
import Random.List
import Styling exposing (defaultMargin)



-- MODEL


type alias Answer =
    PracticeModule.Answer Int


type alias GameState =
    PracticeModule.GameState Int


shuffleCommand : Cmd Msg
shuffleCommand =
    Random.generate Input (Random.List.shuffle (List.range 1 20))



-- UPDATE


type alias Msg =
    PracticeModule.Msg Int



-- VIEW


viewContents model =
    if not (PracticeModule.initialised model) then
        h2 [] [ text "Laden. Bitte warten..." ]

    else
        let
            feedback =
                Maybe.map (\answer -> [ PracticeModule.answerMessage doubling answer ]) model.previous |> Maybe.withDefault []
        in
        case model.remaining of
            currentNumber :: _ ->
                div []
                    [ Keyed.node "form"
                        [ onSubmit Submit ]
                        (List.map (\x -> ( "feedback", x )) feedback
                            ++ [ ( "question", div [] [ text ("Was ist das Doppelte von " ++ String.fromInt currentNumber ++ "?") ] )
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


doubling : PracticeModule Int
doubling =
    { name = "Verdoppeln"
    , init = \_ -> ( { currentValue = Nothing, remaining = [], answered = [], previous = Nothing }, shuffleCommand )
    , viewContents = viewContents
    , defaultTask = 0
    , expected = \task -> task * 2
    , errorMessage = \n -> "Das Doppelte von " ++ String.fromInt n ++ " ist " ++ String.fromInt (n * 2) ++ "."
    }
