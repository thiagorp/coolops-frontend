module App.Pages.Projects.Edit.Main exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Api
import App.Forms.Projects.Main as Form
import App.Html as AppHtml
import App.Pages.NotFound as NotFound
import App.Pages.Projects.Edit.Data as Data
import App.Pages.Projects.Edit.SlackSection as SlackSection
import App.Pages.ServerError as ServerError
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route
import Time
import Util as PageUtil exposing (PageHandler, andPerform, noop, return)


type alias LoadedData =
    { project : Data.Project
    , slack : SlackSection.Model
    , formModel : Form.Model
    , showSuccessMessage : Bool
    }


type Page
    = Loading
    | Loaded LoadedData
    | ProjectNotFound
    | ApiError


type alias Model =
    { page : Page
    , apiConfig : Api.ProtectedConfig
    , projectId : String
    , navigationKey : Route.NavigationKey
    }


type Msg
    = FormMsg Form.Msg
    | SlackSectionMsg SlackSection.Msg
    | DataLoaded (Api.ApiResult Data.Response)
    | DismissSuccessMessage



-- Init


init : Api.ProtectedConfig -> Route.NavigationKey -> String -> PageHandler Model Msg
init apiConfig navigationKey projectId =
    return
        { apiConfig = apiConfig
        , projectId = projectId
        , page = Loading
        , navigationKey = navigationKey
        }
        |> andPerform (Data.getData apiConfig projectId DataLoaded)



-- Update


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    let
        setFormModel pageModel newFormModel =
            { model | page = Loaded { pageModel | formModel = newFormModel } }

        setSlackModel pageModel newModel =
            { model | page = Loaded { pageModel | slack = newModel } }

        initLoadedPage slackModel project newFormModel =
            { model | page = Loaded { project = project, slack = slackModel, showSuccessMessage = False, formModel = newFormModel } }
    in
    case msg of
        FormMsg subMsg ->
            case model.page of
                Loaded data ->
                    case subMsg of
                        Form.SubmitResponse (Ok _) ->
                            Form.update subMsg data.formModel
                                |> PageUtil.map (setFormModel { data | showSuccessMessage = True }) FormMsg

                        Form.Submit ->
                            Form.update subMsg data.formModel
                                |> PageUtil.map (setFormModel { data | showSuccessMessage = False }) FormMsg

                        _ ->
                            Form.update subMsg data.formModel
                                |> PageUtil.map (setFormModel data) FormMsg

                _ ->
                    return model

        DataLoaded (Ok response) ->
            case response.project of
                Just project ->
                    let
                        ( formModel, formCmds ) =
                            Form.init model.apiConfig (Form.Update project)
                                |> PageUtil.map identity FormMsg

                        ( slackModel, slackCmds ) =
                            SlackSection.init model.apiConfig model.navigationKey project response.slackAccessToken response.slackConfiguration False
                                |> PageUtil.map identity SlackSectionMsg
                    in
                    return (initLoadedPage slackModel project formModel)
                        |> PageUtil.andPerform_ (formCmds ++ slackCmds)

                Nothing ->
                    return { model | page = ProjectNotFound }

        DismissSuccessMessage ->
            case model.page of
                Loaded data ->
                    return { model | page = Loaded { data | showSuccessMessage = False } }

                _ ->
                    return model

        DataLoaded (Err _) ->
            return { model | page = ApiError }

        SlackSectionMsg subMsg ->
            case model.page of
                Loaded data ->
                    SlackSection.update subMsg data.slack
                        |> PageUtil.map (setSlackModel data) SlackSectionMsg

                _ ->
                    return model



-- View


slackIntegration : SlackSection.Model -> Html Msg
slackIntegration subModel =
    SlackSection.view subModel
        |> Html.map SlackSectionMsg


ciIntegration : Data.Project -> Html Msg
ciIntegration { accessToken } =
    div [ class "card" ]
        [ div [ class "card-header" ]
            [ h3 [ class "card-title" ] [ text "CI Integration" ]
            ]
        , div [ class "card-body small" ]
            [ p []
                [ text "Following the instructions of our "
                , a [ href "https://github.com/coolopsio/coolops", target "_blank" ] [ text "CLI library" ]
                , text " to integrate with your favorite CI."
                ]
            , h4 [ class "mt-6" ] [ text "Project info" ]
            , p []
                [ dl [ class "row" ]
                    [ dt [ class "col-md-3 col-lg-12 col-xl-3" ] [ text "Access token" ]
                    , dd [ class "col-md-9 col-lg-12 col-xl-9" ] [ code [] [ text accessToken ] ]
                    ]
                ]
            ]
        ]


projectForm : LoadedData -> Html Msg
projectForm { showSuccessMessage, formModel } =
    Html.form [ class "card", onSubmit Form.Submit ]
        [ div [ class "card-header" ]
            [ h3 [ class "card-title" ] [ text "Edit project information" ]
            ]
        , div [ class "card-body" ]
            (Form.view formModel)
        , div [ class "card-footer text-right" ]
            [ span [ class "text-success mr-4 small font-weight-light", classList [ ( "d-none", not showSuccessMessage ) ] ] [ text "Saved with success" ]
            , button [ type_ "submit", class "btn btn-primary", disabled (Form.isSubmitting formModel) ] [ text "Save" ]
            ]
        ]
        |> Html.map FormMsg


view : Model -> Html Msg
view model =
    case model.page of
        Loading ->
            div [] []

        ProjectNotFound ->
            NotFound.view

        ApiError ->
            ServerError.view

        Loaded data ->
            AppHtml.container
                [ AppHtml.pageHeader "Project settings"
                , div [ class "row" ]
                    [ div [ class "col-lg-7 col-xs-12" ]
                        [ projectForm data ]
                    , div [ class "col-lg-5 col-xs-12" ]
                        [ slackIntegration data.slack
                        , ciIntegration data.project
                        ]
                    ]
                ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Loaded data ->
            let
                pageSub =
                    case data.showSuccessMessage of
                        True ->
                            Time.every (5 * 1000) (\_ -> DismissSuccessMessage)

                        False ->
                            Sub.none

                slackSub =
                    SlackSection.subscriptions data.slack
                        |> Sub.map SlackSectionMsg
            in
            Sub.batch [ pageSub, slackSub ]

        _ ->
            Sub.none
