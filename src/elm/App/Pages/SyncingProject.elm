module App.Pages.SyncingProject exposing (..)

import App.Api.ConnectProjectWithSlack exposing (..)
import Html exposing (..)
import Http
import Route exposing (..)
import Util exposing (PageHandler, andPerform, noop, return)


type alias Model =
    { projectId : String }


type Msg
    = SyncResponse (Result Http.Error ())


init : String -> String -> Maybe String -> Maybe String -> PageHandler Model Msg
init baseUrl apiToken maybeCode maybeState =
    let
        code =
            Maybe.withDefault "" maybeCode

        state =
            Maybe.withDefault "" maybeState
    in
    return { projectId = state }
        |> andPerform (connectProjectWithSlack baseUrl apiToken SyncResponse { code = code, projectId = state })


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        SyncResponse _ ->
            return model
                |> andPerform (redirectTo (Protected (EditProject model.projectId)))


view : Model -> Html Msg
view _ =
    div [] []
