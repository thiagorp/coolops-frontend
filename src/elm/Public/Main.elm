module Public.Main exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Html
import Navigation
import Public.Pages.DeploymentLogs as DeploymentLogs
import Public.Pages.Login as Login
import Public.Pages.NotFound as NotFound
import Public.Pages.Signup as Signup
import Route


type Msg
    = UrlChanged Navigation.Location
    | SignupMsg Signup.Msg
    | LoginMsg Login.Msg
    | DeploymentLogsMsg DeploymentLogs.Msg


type Page
    = NotFound
    | Signup Signup.Model
    | Login Login.Model
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


setPage : Model -> Navigation.Location -> ( Model, Cmd Msg )
setPage model location =
    case Route.readOpenRoute location of
        Just Route.Signup ->
            wrapPage Signup SignupMsg model (Signup.init model.baseUrl)

        Just Route.Login ->
            wrapPage Login LoginMsg model (Login.init model.baseUrl)

        Just (Route.DeploymentLogs deploymentId) ->
            wrapPage DeploymentLogs DeploymentLogsMsg model (DeploymentLogs.init model.baseUrl deploymentId)

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )


init : String -> Navigation.Location -> ( Model, Cmd Msg )
init baseUrl =
    setPage (Model NotFound baseUrl)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged location ->
            setPage model location

        SignupMsg subMsg ->
            case model.page of
                Signup subModel ->
                    Signup.update subMsg subModel
                        |> wrapPage Signup SignupMsg model

                _ ->
                    ( model, Cmd.none )

        LoginMsg subMsg ->
            case model.page of
                Login subModel ->
                    Login.update subMsg subModel
                        |> wrapPage Login LoginMsg model

                _ ->
                    ( model, Cmd.none )

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
        Signup subModel ->
            Signup.view subModel
                |> Html.map SignupMsg

        Login subModel ->
            Login.view subModel
                |> Html.map LoginMsg

        DeploymentLogs subModel ->
            DeploymentLogs.view subModel
                |> Html.map DeploymentLogsMsg

        NotFound ->
            NotFound.view


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        NotFound ->
            Sub.none

        Login _ ->
            Sub.none

        Signup _ ->
            Sub.none

        DeploymentLogs subModel ->
            DeploymentLogs.subscriptions subModel
                |> Sub.map DeploymentLogsMsg
