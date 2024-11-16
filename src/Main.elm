module Main exposing (..)

import Bootstrap.Alert exposing (simpleDanger, simpleSuccess)
import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (Html, br, button, div, fromUnstyled, h1, h2, input, text, toUnstyled)
import Html.Styled.Attributes as A exposing (autofocus, class, css, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Html.Styled.Keyed as Keyed
import Random
import SumTask exposing (Task, fromInt)



-- MAIN


main =
    Browser.element { init = init, update = update, view = view >> toUnstyled, subscriptions = \_ -> Sub.none }



-- MODEL


type Answer
    = Correct
    | Wrong Task


isCorrect : Answer -> Bool
isCorrect answer =
    case answer of
        Correct ->
            True

        _ ->
            False


type alias GameState =
    { currentValue : Maybe Int
    , remaining : List Task
    , answered : List Answer
    , previous : Maybe Answer
    }


initialised : GameState -> Bool
initialised gamestate =
    not (List.isEmpty gamestate.remaining && List.isEmpty gamestate.answered)


buildTasks : Cmd Msg
buildTasks =
    Random.generate (\list -> Input (List.map fromInt list)) (Random.list 20 (Random.int 1000 1000000))


init : () -> ( GameState, Cmd Msg )
init _ =
    ( { currentValue = Nothing, remaining = [], answered = [], previous = Nothing }, buildTasks )



-- UPDATE


type Msg
    = Change String
    | Submit
    | Input (List Task)
    | Reset


update : Msg -> GameState -> ( GameState, Cmd Msg )
update msg gameState =
    case msg of
        Reset ->
            init ()

        Change val ->
            ( { gameState | currentValue = String.toInt val }, Cmd.none )

        Submit ->
            let
                currentTask =
                    List.head gameState.remaining |> Maybe.withDefault (fromInt 0)

                expected =
                    currentTask.sum

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


view : GameState -> Html Msg
view model =
    div
        [ css
            [ marginLeft (pct 30)
            , marginRight (pct 30)
            , marginTop (px 20)
            , paddingBottom (px 12)
            , paddingTop (px 1)
            , textAlign center
            , backgroundColor (hex "#FFF")
            , boxShadow4 (px 0) (px 30) (px 60) (rgba 0 0 0 0.3)
            , borderRadius (px 12)
            ]
        ]
        [ h1 [ css [ paddingTop (px 4) ] ] [ text "Große Summen" ]
        , div [] [ view1 model ]
        ]


defaultMargin : List Style
defaultMargin =
    [ margin (px 3) ]


view1 : GameState -> Html Msg
view1 model =
    if not (initialised model) then
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
