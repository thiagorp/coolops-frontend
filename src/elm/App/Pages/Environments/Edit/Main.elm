module App.Pages.Environments.Edit.Main exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Api
import App.Pages.Environments.Edit.Data as Data
import App.Pages.Environments.Edit.Form as Form
import App.Pages.NotFound as NotFound
import App.Pages.ServerError as ServerError
import Html exposing (..)
import Route
import Util as PageUtil exposing (PageHandler, andPerform, noop, return)


type Page
    = Loading
    | Form Form.Model
    | NotFound
    | ApiError


type alias Model =
    { page : Page
    , baseUrl : String
    , apiToken : String
    , id : String
    , navigationKey : Route.NavigationKey
    }


type Msg
    = FormMsg Form.Msg
    | DataLoaded (Api.ApiResult (Maybe Data.Environment))


init : String -> String -> Route.NavigationKey -> String -> PageHandler Model Msg
init baseUrl apiToken navigationKey id =
    return
        { apiToken = apiToken
        , baseUrl = baseUrl
        , id = id
        , page = Loading
        , navigationKey = navigationKey
        }
        |> andPerform (Data.getEnvironment baseUrl apiToken id DataLoaded)


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
            case response of
                Just environment ->
                    let
                        ( subModel, cmd ) =
                            Form.init model.baseUrl model.apiToken model.navigationKey environment
                                |> PageUtil.map Form FormMsg
                    in
                    ( { model | page = subModel }, cmd )

                Nothing ->
                    return { model | page = NotFound }

        DataLoaded (Err _) ->
            return { model | page = ApiError }


view : Model -> Html Msg
view model =
    case model.page of
        Loading ->
            div [] []

        NotFound ->
            NotFound.view

        ApiError ->
            ServerError.view

        Form subModel ->
            Form.view subModel
                |> Html.map FormMsg
