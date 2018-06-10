module App.Pages.ProjectsList exposing (..)

import Html exposing (Html, h1, text)
import Util exposing (PageHandler, andPerform, noop, return)


type alias Model =
    {}


init : PageHandler Model Msg
init =
    return {}


type Msg
    = NoOp


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    return model


view : Model -> Html Msg
view model =
    h1 [] [ text "Lala" ]
