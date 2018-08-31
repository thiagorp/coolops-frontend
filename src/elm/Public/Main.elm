module Public.Main exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Html
import Public.Pages.DeploymentLogs.Main as DeploymentLogs
import Route


type Msg
    = UrlChanged Route.PublicRoute
    | DeploymentLogsMsg DeploymentLogs.Msg


type Page
    = Transitioning
    | DeploymentLogs DeploymentLogs.Model


type alias Model =
    { page : Page, baseUrl : String }


wrapPage : (model -> Page) -> (msg -> Msg) -> Model -> ( model, List (Cmd msg) ) -> ( Model, Cmd Msg )
wrapPage toPage toMsg model ( subModel, subCmds ) =
    let
        subCmd =
            case subCmds of
                [] ->
                    Cmd.none

                cmds ->
                    Cmd.batch subCmds

        cmd =
            Cmd.map toMsg subCmd

        page =
            toPage subModel
    in
    ( { model | page = page }, cmd )


setPage : Model -> Route.PublicRoute -> ( Model, Cmd Msg )
setPage model route =
    case route of
        Route.DeploymentLogs id ->
            wrapPage DeploymentLogs DeploymentLogsMsg model (DeploymentLogs.init model.baseUrl id)


init : String -> Route.PublicRoute -> ( Model, Cmd Msg )
init baseUrl =
    setPage (Model Transitioning baseUrl)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged page ->
            setPage model page

        DeploymentLogsMsg subMsg ->
            case model.page of
                DeploymentLogs subModel ->
                    DeploymentLogs.update subMsg subModel
                        |> wrapPage DeploymentLogs DeploymentLogsMsg model

                _ ->
                    ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    case model.page of
        Transitioning ->
            Html.div [] []

        DeploymentLogs subModel ->
            DeploymentLogs.view subModel
                |> Html.map DeploymentLogsMsg


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        DeploymentLogs subModel ->
            DeploymentLogs.subscriptions subModel
                |> Sub.map DeploymentLogsMsg

        Transitioning ->
            Sub.none
