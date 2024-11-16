module Main exposing (..)

import Bootstrap.Alert exposing (simpleDanger, simpleSuccess)
import Browser
import Html
import Html.Styled exposing (Html, div, fromUnstyled, h2, input, text, toUnstyled)
import Html.Styled.Attributes exposing (class, css, type_, value)
import Html.Styled.Events exposing (onSubmit)
import Html.Styled.Keyed as Keyed
import LargeSums.LargeSums as LargeSums
import Styling exposing (defaultMargin, mainWindow)
import SumTask exposing (Task)



-- MAIN


main =
    Browser.element { init = init, update = update, view = view >> toUnstyled, subscriptions = \_ -> Sub.none }



-- MODEL


type GameState
    = Init
    | Error String
    | LargeSumsState LargeSums.GameState


init : () -> ( GameState, Cmd Msg )
init _ =
    ( Init, Cmd.none )



-- UPDATE


type Msg
    = LargeSumsMsg LargeSums.Msg


update : Msg -> GameState -> ( GameState, Cmd Msg )
update commonMsg gameState =
    case ( commonMsg, gameState ) of
        ( LargeSumsMsg msg, LargeSumsState state ) ->
            let
                ( foo, bar ) =
                    LargeSums.update msg state
            in
            ( LargeSumsState foo, Cmd.map LargeSumsMsg bar )

        ( LargeSumsMsg LargeSums.Reset, Init ) ->
            let
                ( foo, bar ) =
                    LargeSums.init ()
            in
            ( LargeSumsState foo, Cmd.map LargeSumsMsg bar )

        _ ->
            ( Error "Game state and message do not match. This should not happen.", Cmd.none )



-- VIEW


view : GameState -> Html Msg
view model =
    case model of
        LargeSumsState state ->
            Html.Styled.map LargeSumsMsg (LargeSums.view state)

        Init ->
            let
                contents =
                    div []
                        [ text "Bitte wähle eine Übung:"
                        , Keyed.node "form"
                            [ onSubmit (LargeSumsMsg LargeSums.Reset) ]
                            [ ( "submitButton", input [ type_ "submit", css defaultMargin, value "Große Summen", class "btn btn-primary" ] [] ) ]
                        ]
            in
            mainWindow "Mathe" contents

        Error message ->
            h2 [] [ text (String.append "Error: " message) ]
