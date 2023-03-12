module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (type_, value)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Model = {
        currentValue: Maybe Int,
        remaining: List Int,
        answered: List (Int, Bool)
    }

init : Model
init = { 
    currentValue = Nothing,
    remaining = List.range 1 20,
    answered = []
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
          correct = (Maybe.withDefault -1 model.currentValue) == expected
          updatedAnswers = (currentNumber, correct) :: model.answered
       in {
              currentValue = Nothing,
              remaining = List.tail model.remaining |> Maybe.withDefault [],
              answered = updatedAnswers
          }


-- VIEW

view : Model -> Html Msg
view model =
  case List.head model.remaining of
    Just currentNumber ->
        div []
                [ div [] [ text ("Was ist das doppelte von " ++ String.fromInt currentNumber)]
                , input [ onInput Change, type_ "number", value ( (Maybe.map String.fromInt model.currentValue) |> Maybe.withDefault "" ) ] []
                , button [ onClick Submit ] [ text "Ok" ]
                ]
    Nothing -> 
        let numbers = List.length model.answered
            correctAnswers = List.length ( List.filter Tuple.second model.answered)
         in div []
                [ div [] [ text "Game over.\n" ]
                , div [] [ text ("Du hast " ++ (String.fromInt correctAnswers) ++ " von " ++ (String.fromInt numbers)  ++ " Aufgaben korrekt gelöst!") ]
                ]