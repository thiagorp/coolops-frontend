module App.Pages.Projects.New.SlackIntegration.Callback exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Api
import App.Api.ConnectProjectWithSlack exposing (..)
import App.Html exposing (spinner)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Route exposing (..)
import Util exposing (PageHandler, andPerform, noop, return)


type alias Model =
    { navigationKey : NavigationKey
    , projectId : String
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
        SyncResponse (Ok _) ->
            return model
                |> andPerform (modifyTo model.navigationKey (Protected (NewProject (CreateEnvironments model.projectId))))

        SyncResponse (Err _) ->
            return model
                |> andPerform (modifyTo model.navigationKey (Protected (NewProject (IntegrateWithSlack model.projectId True))))


view : Model -> Html Msg
view _ =
    div [ class "card" ]
        [ div [ class "card-body" ]
            [ spinner ]
        ]
