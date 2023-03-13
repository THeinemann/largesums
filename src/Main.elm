module Main exposing (..)

import Browser
import Html.Styled exposing (Html, div, text, input, br, h1, toUnstyled)
import Html.Styled.Events exposing (onSubmit, onInput)
import Html.Styled.Attributes exposing (type_, value, class, autofocus, css)
import Random.List
import Random
import Html.Styled.Keyed as Keyed
import Css exposing (..)

-- MAIN


main =
  Browser.element { init = init, update = updateWrapper, view = view >> toUnstyled, subscriptions = (\_ -> Sub.none) }



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

init : () -> (Model, Cmd Msg)
init _ = ({ 
    currentValue = Nothing,
    remaining = [],
    answered = [],
    previous = Nothing
 }, Random.generate Input (Random.List.shuffle (List.range 1 20)) )


-- UPDATE

type Msg = Change String | Submit | Input (List Int)


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
    Input list ->
      { model | remaining = list }


updateWrapper : Msg -> Model -> (Model, Cmd Msg)
updateWrapper msg model = (update msg model, Cmd.none)


-- VIEW

answerMessage : Answer -> Html msg
answerMessage answer =
    case answer of
        Correct -> div [ class "feedback correct" ] [ text "Richtig!" ]
        Wrong n -> div [ class "feedback wrong" ] [ text ("Leider Falsch. Das doppelte von " ++ (String.fromInt n) ++ " ist " ++ (String.fromInt (n * 2)) ++ "!") ]

view : Model -> Html Msg
view model =
  div [ css [ marginLeft (pct 30), marginRight (pct 30), height (pct 100), textAlign center ] ]
    [ h1 [] [ text "Verdoppeln" ]
    , div [] [view1 model]
    ]

defaultMargin = [ margin (px 3) ]

view1 : Model -> Html Msg
view1 model =
  let feedback = Maybe.map (\answer -> [ answerMessage answer ]) model.previous |> Maybe.withDefault []
   in case List.head model.remaining of
        Just currentNumber ->
            div []
                    [ Keyed.node "form" [ onSubmit Submit ]
                        ((List.map (\x -> ("feedback", x)) feedback) ++ 
                          [ ("question", div [] [ text ("Was ist das Doppelte von " ++ String.fromInt currentNumber ++ "?")])
                          , ("input", input [ onInput Change, type_ "number", value ( (Maybe.map String.fromInt model.currentValue) |> Maybe.withDefault "" ), css defaultMargin, autofocus True ] [])
                          , ("br", br [] [])
                          , ("submitButton", input [ type_ "submit", css defaultMargin ] [ text "Ok" ])
                          ]
                        )
                    ]
        Nothing -> 
            let numbers = List.length model.answered
                correctAnswers = List.length ( List.filter isCorrect model.answered)
            in div []
                    (feedback ++ [ div [] [ text "Game over.\n" ]
                    , div [] [ text ("Du hast " ++ (String.fromInt correctAnswers) ++ " von " ++ (String.fromInt numbers)  ++ " Aufgaben korrekt gelöst!") ]
                    ])