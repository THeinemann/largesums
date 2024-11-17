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


update : Msg -> GameState -> ( GameState, Cmd Msg )
update msg gameState =
    case msg of
        Reset ->
            doubling.init ()

        Change val ->
            ( { gameState | currentValue = String.toInt val }, Cmd.none )

        Submit ->
            let
                currentNumber =
                    List.head gameState.remaining |> Maybe.withDefault 0

                expected =
                    currentNumber * 2

                answer =
                    if Maybe.withDefault -1 gameState.currentValue == expected then
                        Correct

                    else
                        Wrong currentNumber

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


answerMessage : Answer -> Html msg
answerMessage answer =
    let
        unstyled =
            case answer of
                Correct ->
                    simpleSuccess [] [ Html.text "Richtig!" ]

                Wrong n ->
                    simpleDanger [] [ Html.text ("Leider Falsch. Das Doppelte von " ++ String.fromInt n ++ " ist " ++ String.fromInt (n * 2) ++ ".") ]
    in
    fromUnstyled unstyled


viewContents model =
    if not (PracticeModule.initialised model) then
        h2 [] [ text "Laden. Bitte warten..." ]

    else
        let
            feedback =
                Maybe.map (\answer -> [ answerMessage answer ]) model.previous |> Maybe.withDefault []
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
    , update = update
    , viewContents = viewContents
    }
