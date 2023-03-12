module Main exposing (..)

import Browser
import Html exposing (Html, div, text, input)
import Html.Events exposing (onSubmit, onInput)
import Html.Attributes exposing (type_, value)
import Html exposing (form)

-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type Answer = Correct | Wrong Int

isCorrect : Answer -> Bool
isCorrect answer = 
    case answer of
        Correct -> True
        _ -> False

type alias Model = {
        currentValue: Maybe Int,
        remaining: List Int,
        answered: List (Answer),
        previous: Maybe Answer
    }

init : Model
init = { 
    currentValue = Nothing,
    remaining = List.range 1 20,
    answered = [],
    previous = Nothing
 }


-- UPDATE

type Msg = Change String | Submit

update : Msg -> Model -> Model
update msg model =
  case msg of
    Change val ->
      { model | currentValue = String.toInt val }

    Submit ->
      let currentNumber = List.head model.remaining |> Maybe.withDefault 0
          expected = currentNumber * 2
          answer = if (Maybe.withDefault -1 model.currentValue) == expected then Correct else Wrong currentNumber
          updatedAnswers = answer :: model.answered
       in {
              currentValue = Nothing,
              remaining = List.tail model.remaining |> Maybe.withDefault [],
              answered = updatedAnswers,
              previous = Just (answer)
          }


-- VIEW

answerMessage : Answer -> Html msg
answerMessage answer =
    case answer of
        Correct -> div [] [ text "Richtig!" ]
        Wrong n -> div [] [ text ("Leider Falsch. Das doppelte von " ++ (String.fromInt n) ++ " ist " ++ (String.fromInt (n * 2)) ++ "!") ]

view : Model -> Html Msg
view model =
  let feedback = Maybe.map (\answer -> [ answerMessage answer ]) model.previous |> Maybe.withDefault []
   in case List.head model.remaining of
        Just currentNumber ->
            div []
                    [ form [ onSubmit Submit ]
                        (feedback ++ [ div [] [ text ("Was ist das doppelte von " ++ String.fromInt currentNumber)]
                        , input [ onInput Change, type_ "number", value ( (Maybe.map String.fromInt model.currentValue) |> Maybe.withDefault "" ) ] []
                        , input [ type_ "submit" ] [ text "Ok" ]
                        ])
                    ]
        Nothing -> 
            let numbers = List.length model.answered
                correctAnswers = List.length ( List.filter isCorrect model.answered)
            in div []
                    (feedback ++ [ div [] [ text "Game over.\n" ]
                    , div [] [ text ("Du hast " ++ (String.fromInt correctAnswers) ++ " von " ++ (String.fromInt numbers)  ++ " Aufgaben korrekt gelöst!") ]
                    ])