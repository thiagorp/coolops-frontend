module Auth.Main exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Api
import Auth.Pages.Login as Login
import Auth.Pages.NotFound as NotFound
import Auth.Pages.Signup as Signup
import Html
import Route


type Msg
    = UrlChanged Route.AuthRoute
    | SignupMsg Signup.Msg
    | LoginMsg Login.Msg


type Page
    = NotFound
    | Signup Signup.Model
    | Login Login.Model


type alias Model =
    { page : Page, apiConfig : Api.PublicConfig }


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


setPage : Model -> Route.AuthRoute -> ( Model, Cmd Msg )
setPage model page =
    case page of
        Route.Signup ->
            wrapPage Signup SignupMsg model (Signup.init model.apiConfig)

        Route.Login ->
            wrapPage Login LoginMsg model (Login.init model.apiConfig)


init : Api.PublicConfig -> Route.AuthRoute -> ( Model, Cmd Msg )
init apiConfig =
    setPage (Model NotFound apiConfig)


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


view : Model -> Html.Html Msg
view model =
    case model.page of
        Signup subModel ->
            Signup.view subModel
                |> Html.map SignupMsg

        Login subModel ->
            Login.view subModel
                |> Html.map LoginMsg

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
