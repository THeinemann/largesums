module PracticeModule exposing (..)

import Bootstrap.Alert exposing (simpleDanger, simpleSuccess)
import Html
import Html.Styled exposing (Html, br, button, div, fromUnstyled, h2, input, text)
import Html.Styled.Attributes as A exposing (autofocus, class, css, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Html.Styled.Keyed as Keyed
import Styling exposing (defaultMargin, mainWindow)



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
    , defaultTask : task
    , question : task -> String
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


viewContents : PracticeModule task -> GameState task -> Html (Msg task)
viewContents mod model =
    if not (initialised model) then
        h2 [] [ text "Laden. Bitte warten..." ]

    else
        let
            feedback =
                Maybe.map (\answer -> [ answerMessage mod answer ]) model.previous |> Maybe.withDefault []
        in
        case model.remaining of
            currentTask :: _ ->
                div []
                    [ Keyed.node "form"
                        [ onSubmit Submit ]
                        (List.map (\x -> ( "feedback", x )) feedback
                            ++ [ ( "question", div [] [ text (mod.question currentTask) ] )
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


view : PracticeModule task -> GameState task -> Html (Msg task)
view mod model =
    mainWindow mod.name (viewContents mod model)
