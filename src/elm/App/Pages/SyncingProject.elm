module App.Pages.SyncingProject exposing (Model, Msg(..), init, update, view)

import App.Api.ConnectProjectWithSlack exposing (..)
import Html exposing (..)
import Http
import Route exposing (..)
import Util exposing (PageHandler, andPerform, noop, return)


type alias Model =
    { projectId : String }


type Msg
    = SyncResponse (Result Http.Error ())


init : String -> String -> String -> String -> PageHandler Model Msg
init baseUrl apiToken code projectId =
    return { projectId = projectId }
        |> andPerform (connectProjectWithSlack baseUrl apiToken SyncResponse { code = code, projectId = projectId })


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        SyncResponse _ ->
            return model
                |> andPerform (redirectTo (Protected (EditProject model.projectId)))


view : Model -> Html Msg
view _ =
    div [] []
