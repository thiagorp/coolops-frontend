module App.Pages.SyncingProject exposing (Model, Msg(..), init, update, view)

import Api
import App.Api.ConnectProjectWithSlack exposing (..)
import Html exposing (..)
import Http
import Route exposing (..)
import Util exposing (PageHandler, andPerform, noop, return)


type alias Model =
    { projectId : String
    , navigationKey : NavigationKey
    }


type Msg
    = SyncResponse (Result Http.Error ())


init : Api.ProtectedConfig -> NavigationKey -> String -> String -> PageHandler Model Msg
init apiConfig navigationKey code projectId =
    return { projectId = projectId, navigationKey = navigationKey }
        |> andPerform (connectProjectWithSlack apiConfig SyncResponse { code = code, projectId = projectId })


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        SyncResponse _ ->
            return model
                |> andPerform (redirectTo model.navigationKey (Protected (EditProject model.projectId)))


view : Model -> Html Msg
view _ =
    div [] []
