module App.Pages.Projects.New.SlackIntegration.Main exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Api
import App.Forms.ProjectSlackIntegrations.Authorize as Authorize
import App.Forms.ProjectSlackIntegrations.SelectChannel as SelectChannel
import App.Html.Form as Form
import App.Pages.Projects.New.SlackIntegration.Data as Data
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Route
import Util exposing (PageHandler, andPerform, noop, return)



-- Model


type Page
    = Loading
    | Error
    | Authorize Authorize.Model
    | SelectChannel SelectChannel.Model


type alias Model =
    { apiConfig : Api.ProtectedConfig
    , page : Page
    , error : Bool
    , navigationKey : Route.NavigationKey
    }



-- Msg


type Msg
    = DataLoaded (Api.ApiResult Data.Response)
    | AuthorizeMsg Authorize.Msg
    | SelectChannelMsg SelectChannel.Msg



-- Init


init : Api.ProtectedConfig -> Route.NavigationKey -> String -> Bool -> PageHandler Model Msg
init apiConfig navigationKey projectId error =
    return
        { apiConfig = apiConfig
        , error = error
        , navigationKey = navigationKey
        , page = Loading
        }
        |> andPerform (Data.getData apiConfig projectId DataLoaded)



-- Update


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        DataLoaded (Ok result) ->
            case result.project of
                Nothing ->
                    return model
                        |> andPerform (Route.redirectTo model.navigationKey (Route.Protected (Route.NewProject Route.CreateProject)))

                Just project ->
                    case result.slackAccessToken of
                        Just accessToken ->
                            SelectChannel.init model.apiConfig accessToken project
                                |> Util.map (\m -> { model | page = SelectChannel m }) SelectChannelMsg

                        Nothing ->
                            Authorize.init model.navigationKey result.slackConfiguration project model.error "newProject|"
                                |> Util.map (\m -> { model | page = Authorize m }) AuthorizeMsg

        DataLoaded (Err _) ->
            return { model | page = Error }

        AuthorizeMsg subMsg ->
            case model.page of
                Authorize subModel ->
                    Authorize.update subMsg subModel
                        |> Util.map (\m -> { model | page = Authorize m }) AuthorizeMsg

                _ ->
                    return model

        SelectChannelMsg subMsg ->
            case model.page of
                SelectChannel subModel ->
                    case subMsg of
                        SelectChannel.SubmitResponse (Ok _) ->
                            return model
                                |> andPerform (Route.redirectTo model.navigationKey (Route.Protected (Route.NewProject (Route.CreateEnvironments subModel.projectId))))

                        _ ->
                            SelectChannel.update subMsg subModel
                                |> Util.map
                                    (\m -> { model | page = SelectChannel m })
                                    SelectChannelMsg

                _ ->
                    return model



-- View


view : Model -> Html Msg
view model =
    case model.page of
        Loading ->
            div [] []

        Error ->
            div [] [ text "Failed to load data" ]

        Authorize subModel ->
            div [ class "card" ]
                [ div [ class "card-body" ] (Authorize.view subModel)
                ]
                |> Html.map AuthorizeMsg

        SelectChannel subModel ->
            div [ class "card" ]
                [ Html.form [ onSubmit SelectChannel.Submit ]
                    [ div [ class "card-body" ] (SelectChannel.view subModel)
                    , div [ class "card-footer text-right" ]
                        [ Form.submitButton "Continue" (SelectChannel.isSubmitting subModel)
                        ]
                    ]
                ]
                |> Html.map SelectChannelMsg
