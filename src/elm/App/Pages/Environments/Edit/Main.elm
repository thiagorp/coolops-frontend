module App.Pages.Environments.Edit.Main exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Api
import App.Forms.Environments.Main as Form
import App.Pages.Environments.Edit.Data as Data
import App.Pages.NotFound as NotFound
import App.Pages.ServerError as ServerError
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route
import Util as PageUtil exposing (PageHandler, andPerform, noop, return)


type Page
    = Loading
    | Form Form.Model
    | NotFound
    | ApiError


type alias Model =
    { page : Page
    , apiConfig : Api.ProtectedConfig
    , id : String
    , navigationKey : Route.NavigationKey
    }


type Msg
    = FormMsg Form.Msg
    | DataLoaded (Api.ApiResult Data.Response)


init : Api.ProtectedConfig -> Route.NavigationKey -> String -> PageHandler Model Msg
init apiConfig navigationKey id =
    return
        { apiConfig = apiConfig
        , id = id
        , page = Loading
        , navigationKey = navigationKey
        }
        |> andPerform (Data.getEnvironment apiConfig id DataLoaded)


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        FormMsg subMsg ->
            case subMsg of
                Form.SubmitResponse (Ok _) ->
                    return model
                        |> andPerform (Route.redirectTo model.navigationKey (Route.Protected Route.ProjectsList))

                _ ->
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
            case response.environment of
                Just environment ->
                    let
                        ( subModel, cmd ) =
                            Form.init model.apiConfig (Form.Update environment) response.formData
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
            div [ class "container" ]
                [ div [ class "page-header" ]
                    [ h1 [ class "page-title" ] [ text ("Edit " ++ subModel.name) ] ]
                , Html.form [ class "card", onSubmit Form.Submit ]
                    [ div [ class "card-body" ]
                        [ Form.view subModel
                        ]
                    , div [ class "card-footer text-right" ]
                        [ button [ type_ "submit", class "btn btn-primary" ]
                            [ text "Save" ]
                        ]
                    ]
                    |> Html.map FormMsg
                ]
