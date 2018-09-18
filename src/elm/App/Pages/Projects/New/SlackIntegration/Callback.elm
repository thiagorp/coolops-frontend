module App.Pages.Projects.New.SlackIntegration.Callback exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import App.Api.ConnectProjectWithSlack exposing (..)
import App.Html exposing (spinner)
import Html exposing (..)
import Html.Attributes exposing (..)
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
        SyncResponse (Ok _) ->
            return model
                |> andPerform (modifyTo (Protected (NewProject (CreateEnvironments model.projectId))))

        SyncResponse (Err _) ->
            return model
                |> andPerform (modifyTo (Protected (NewProject (IntegrateWithSlack model.projectId True))))


view : Model -> Html Msg
view _ =
    div [ class "card" ]
        [ div [ class "card-body" ]
            [ spinner ]
        ]
