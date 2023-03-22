module Main exposing (..)

import Browser
import Html.Styled exposing (Html, div, text, input, br, h1, h2, toUnstyled)
import Html.Styled.Events exposing (onSubmit, onInput)
import Html.Styled.Attributes as A exposing (type_, value, class, autofocus, css)
import Random.List
import Random
import Html.Styled.Keyed as Keyed
import Css exposing (..)
import Html.Styled exposing (button)
import Html.Styled.Events exposing (onClick)
import Bootstrap.Alert exposing (simpleSuccess)
import Html.Styled exposing (fromUnstyled)
import Bootstrap.Alert exposing (simpleDanger)
import Html

-- MAIN


main =
  Browser.element { init = init, update = update, view = view >> toUnstyled, subscriptions = (\_ -> Sub.none) }

-- MODEL

type Answer = Correct | Wrong Int

isCorrect : Answer -> Bool
isCorrect answer = 
    case answer of
        Correct -> True
        _ -> False

type alias GameState = {
        currentValue: Maybe Int,
        remaining: List Int,
        answered: List Answer,
        previous: Maybe Answer
    }

initialised : GameState -> Bool
initialised gamestate = not ( (List.isEmpty gamestate.remaining) && (List.isEmpty gamestate.answered) )


shuffleCommand : Cmd Msg
shuffleCommand = Random.generate Input (Random.List.shuffle (List.range 1 20))

init : () -> (GameState, Cmd Msg)
init _ = ({ currentValue = Nothing, remaining = [], answered = [], previous = Nothing }, shuffleCommand)

-- UPDATE

type Msg = Change String | Submit | Input (List Int) | Reset

update : Msg -> GameState -> (GameState, Cmd Msg)
update msg gameState =
  case msg of
    Reset -> init ()
    Change val ->
      ({ gameState | currentValue = String.toInt val }, Cmd.none)
    Submit ->
      let currentNumber = List.head gameState.remaining |> Maybe.withDefault 0
          expected = currentNumber * 2
          answer = if (Maybe.withDefault -1 gameState.currentValue) == expected then Correct else Wrong currentNumber
          updatedAnswers = answer :: gameState.answered
          newModel = {
              currentValue = Nothing,
              remaining = List.tail gameState.remaining |> Maybe.withDefault [],
              answered = updatedAnswers,
              previous = Just (answer)
            }
       in (newModel, Cmd.none)
    Input list ->
      ({gameState | remaining = list}, Cmd.none)

-- VIEW

answerMessage : Answer -> Html msg
answerMessage answer =
    let unstyled = case answer of
            Correct -> simpleSuccess [ ] [ Html.text "Richtig!" ]
            Wrong n -> simpleDanger [ ] [ Html.text ("Leider Falsch. Das Doppelte von " ++ (String.fromInt n) ++ " ist " ++ (String.fromInt (n * 2)) ++ ".") ]
     in fromUnstyled unstyled

view : GameState -> Html Msg
view model =
  div [ css [ marginLeft (pct 30)
            , marginRight (pct 30)
            , marginTop (px 20)
            , paddingBottom (px 12)
            , paddingTop (px 1)
            , textAlign center
            , backgroundColor (hex "#FFF")
            , boxShadow4 (px 0) (px 30) (px 60) (rgba 0 0 0 0.3) 
            , borderRadius (px 12)
            ] ]
    [ h1 [ css [ paddingTop (px 4) ] ] [ text "Verdoppeln" ]
    , div [] [view1 model]
    ]

defaultMargin : List Style
defaultMargin = [ margin (px 3) ]

view1 : GameState -> Html Msg
view1 model =
  if not (initialised model) then h2 [] [text "Laden. Bitte warten..."] else
      let feedback = Maybe.map (\answer -> [ answerMessage answer ]) model.previous |> Maybe.withDefault []
      in case model.remaining of
            currentNumber :: _ ->
                div []
                        [ Keyed.node "form" [ onSubmit Submit ]
                            ((List.map (\x -> ("feedback", x)) feedback) ++ 
                              [ ("question", div [] [ text ("Was ist das Doppelte von " ++ String.fromInt currentNumber ++ "?")])
                              , ("input", input [ onInput Change
                                                , type_ "number"
                                                , value ( (Maybe.map String.fromInt model.currentValue) |> Maybe.withDefault "" )
                                                , css defaultMargin
                                                , autofocus True
                                                , A.required True
                                                ] [])
                              , ("br", br [] [])
                              , ("submitButton", input [ type_ "submit", css defaultMargin, value "Ok", class "btn btn-primary" ] [ ])
                              ]
                            )
                        ]
            [] -> 
                let numbers = List.length model.answered
                    correctAnswers = List.length ( List.filter isCorrect model.answered)
                in div []
                        (feedback ++ [ div [] [ text "Game over.\n" ]
                        , div [] [ text ("Du hast " ++ (String.fromInt correctAnswers) ++ " von " ++ (String.fromInt numbers)  ++ " Aufgaben korrekt gelöst!") ]
                        , button [ onClick Reset, css defaultMargin, class "btn btn-primary" ] [ text "Nochmal!" ]
                        ])
