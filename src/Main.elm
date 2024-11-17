module Main exposing (..)

import Browser
import Html.Styled exposing (Html, br, button, div, h2, span, text, toUnstyled)
import Html.Styled.Attributes exposing (class, css, value)
import Html.Styled.Events exposing (onClick)
import Modules.Doubling as Doubling exposing (doubling)
import Modules.LargeSums as LargeSums exposing (largeSums)
import PracticeModule exposing (Msg(..))
import Styling exposing (defaultMargin, mainWindow)



-- MAIN


main =
    Browser.element { init = init, update = update, view = view >> toUnstyled, subscriptions = \_ -> Sub.none }



-- MODEL


type GameState
    = Init
    | Error String
    | LargeSumsState LargeSums.GameState
    | DoublingState Doubling.GameState


init : () -> ( GameState, Cmd Msg )
init _ =
    ( Init, Cmd.none )



-- UPDATE


type Msg
    = LargeSumsMsg LargeSums.Msg
    | DoublingMsg Doubling.Msg


update : Msg -> GameState -> ( GameState, Cmd Msg )
update commonMsg gameState =
    case ( commonMsg, gameState ) of
        ( LargeSumsMsg msg, LargeSumsState state ) ->
            let
                ( moduleState, moduleMsg ) =
                    PracticeModule.update largeSums msg state
            in
            ( LargeSumsState moduleState, Cmd.map LargeSumsMsg moduleMsg )

        ( LargeSumsMsg Reset, Init ) ->
            let
                ( moduleState, moduleMsg ) =
                    largeSums.init ()
            in
            ( LargeSumsState moduleState, Cmd.map LargeSumsMsg moduleMsg )

        ( DoublingMsg msg, DoublingState state ) ->
            let
                ( moduleState, moduleMsg ) =
                    PracticeModule.update doubling msg state
            in
            ( DoublingState moduleState, Cmd.map DoublingMsg moduleMsg )

        ( DoublingMsg Reset, Init ) ->
            let
                ( moduleState, moduleMsg ) =
                    doubling.init ()
            in
            ( DoublingState moduleState, Cmd.map DoublingMsg moduleMsg )

        _ ->
            ( Error "Game state and message do not match. This should not happen.", Cmd.none )



-- VIEW


selectionButton : String -> Msg -> Html Msg
selectionButton name msg =
    span []
        [ button
            [ onClick msg
            , value name
            , css defaultMargin
            , class "btn btn-info"
            ]
            [ text name ]
        , br [] []
        ]


view : GameState -> Html Msg
view model =
    case model of
        LargeSumsState state ->
            Html.Styled.map LargeSumsMsg (PracticeModule.view largeSums state)

        DoublingState state ->
            Html.Styled.map DoublingMsg (PracticeModule.view doubling state)

        Init ->
            let
                contents =
                    div []
                        [ text "Bitte wähle eine Übung:"
                        , br [] []
                        , selectionButton "Verdoppeln" (DoublingMsg Reset)
                        , selectionButton "Große Summen" (LargeSumsMsg Reset)
                        ]
            in
            mainWindow "Mathe" contents

        Error message ->
            h2 [] [ text (String.append "Error: " message) ]
