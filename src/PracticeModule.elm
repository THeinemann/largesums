module PracticeModule exposing (PracticeModule, view)

import Html.Styled exposing (Html, h2, text)
import Styling exposing (mainWindow)


type alias PracticeModule state msg =
    { name : String
    , init : () -> ( state, Cmd msg )
    , initialised : state -> Bool
    , update : msg -> state -> ( state, Cmd msg )
    , viewContents : state -> Html msg
    }


view : PracticeModule state msg -> state -> Html msg
view mod model =
    let
        contents =
            if not (mod.initialised model) then
                h2 [] [ text "Laden. Bitte warten..." ]

            else
                mod.viewContents model
    in
    mainWindow mod.name contents
