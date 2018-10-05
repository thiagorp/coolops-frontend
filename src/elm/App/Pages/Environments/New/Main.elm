module App.Pages.Environments.New.Main exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import Api
import App.Api.CreateEnvironment as Api
import App.Forms.Environments.Data as FormData
import App.Forms.Environments.Main as Form
import App.Html as AppHtml
import App.Html.Form as Form
import Dict exposing (Dict)
import Form.Validation as Validation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Route
import Util exposing (PageHandler, andPerform, noop, return)
import Util.Slug as Slug


type Remote
    = Loading
    | Loaded Form.Model


type alias Model =
    { apiConfig : Api.ProtectedConfig
    , navigationKey : Route.NavigationKey
    , projectId : String
    , form : Remote
    }


type Msg
    = FormMsg Form.Msg
    | FormDataLoaded (Api.ApiResult FormData.Response)



-- Init


init : Api.ProtectedConfig -> Route.NavigationKey -> String -> PageHandler Model Msg
init apiConfig navigationKey projectId =
    return
        { apiConfig = apiConfig
        , navigationKey = navigationKey
        , projectId = projectId
        , form = Loading
        }
        |> andPerform (FormData.get apiConfig FormDataLoaded)



-- Update


setForm : Model -> Form.Model -> Model
setForm model form =
    { model | form = Loaded form }


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        FormMsg subMsg ->
            case model.form of
                Loaded subModel ->
                    case subMsg of
                        Form.SubmitResponse (Ok _) ->
                            return model
                                |> andPerform
                                    (Route.redirectTo model.navigationKey (Route.Protected Route.ProjectsList))

                        _ ->
                            Form.update subMsg subModel
                                |> Util.map (setForm model) FormMsg

                _ ->
                    return model

        FormDataLoaded (Ok response) ->
            Form.init model.apiConfig (Form.Create model.projectId) response
                |> Util.map (setForm model) FormMsg

        FormDataLoaded (Err _) ->
            Form.init model.apiConfig (Form.Create model.projectId) []
                |> Util.map (setForm model) FormMsg



-- View


content : Model -> Html Msg
content model =
    case model.form of
        Loading ->
            div [ class "card" ]
                [ div [ class "card-body" ]
                    [ AppHtml.spinner ]
                ]

        Loaded subModel ->
            Html.form [ class "card", onSubmit Form.Submit ]
                [ div [ class "card-body" ] [ Form.view subModel ]
                , div [ class "card-footer text-right" ]
                    [ button [ class "btn btn-primary", type_ "submit", disabled (Form.isSubmitting subModel) ] [ text "Save" ]
                    ]
                ]
                |> Html.map FormMsg


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "page-header" ]
            [ h1 [ class "page-title" ] [ text "Create environment" ] ]
        , content model
        ]
