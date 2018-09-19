module App.Pages.Projects.Edit.Main exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Api
import App.Pages.NotFound as NotFound
import App.Pages.Projects.Edit.Data as Data
import App.Pages.Projects.Edit.Form as Form
import App.Pages.ServerError as ServerError
import Html exposing (..)
import Route
import Util as PageUtil exposing (PageHandler, andPerform, noop, return)


type Page
    = Loading
    | Form Form.Model
    | ProjectNotFound
    | ApiError


type alias Model =
    { page : Page
    , baseUrl : String
    , apiToken : String
    , projectId : String
    , navigationKey : Route.NavigationKey
    }


type Msg
    = FormMsg Form.Msg
    | DataLoaded (Api.ApiResult Data.Response)


init : String -> String -> Route.NavigationKey -> String -> PageHandler Model Msg
init baseUrl apiToken navigationKey projectId =
    return
        { apiToken = apiToken
        , baseUrl = baseUrl
        , projectId = projectId
        , page = Loading
        , navigationKey = navigationKey
        }
        |> andPerform (Data.getData baseUrl apiToken projectId DataLoaded)


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        FormMsg subMsg ->
            case model.page of
                Form subModel ->
                    let
                        ( page, cmd ) =
                            Form.update subMsg subModel
                                |> PageUtil.map Form FormMsg
                    in
                    ( { model | page = page }, cmd )

                _ ->
                    return model

        DataLoaded (Ok response) ->
            case response.project of
                Just project ->
                    let
                        ( subModel, cmd ) =
                            Form.init model.baseUrl model.apiToken model.navigationKey project response.slackConfiguration
                                |> PageUtil.map Form FormMsg
                    in
                    ( { model | page = subModel }, cmd )

                Nothing ->
                    return { model | page = ProjectNotFound }

        DataLoaded (Err _) ->
            return { model | page = ApiError }


view : Model -> Html Msg
view model =
    case model.page of
        Loading ->
            div [] []

        ProjectNotFound ->
            NotFound.view

        ApiError ->
            ServerError.view

        Form subModel ->
            Form.view subModel
                |> Html.map FormMsg
