module Public.Main exposing (..)

import Html
import Navigation
import Public.Pages.Login as Login
import Public.Pages.NotFound as NotFound
import Public.Pages.Signup as Signup
import Route


type Msg
    = UrlChanged Navigation.Location
    | SignupMsg Signup.Msg
    | LoginMsg Login.Msg


type Page
    = NotFound
    | Signup Signup.Model
    | Login Login.Model


type alias Model =
    { page : Page }


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
            wrapPage Signup SignupMsg model Signup.init

        Just Route.Login ->
            wrapPage Login LoginMsg model Login.init

        _ ->
            ( { page = NotFound }, Cmd.none )


init : Navigation.Location -> ( Model, Cmd Msg )
init =
    setPage (Model NotFound)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( UrlChanged location, _ ) ->
            setPage model location

        ( SignupMsg subMsg, Signup subModel ) ->
            Signup.update subMsg subModel
                |> wrapPage Signup SignupMsg model

        ( LoginMsg subMsg, Login subModel ) ->
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


main : Program Never Model Msg
main =
    Navigation.program UrlChanged
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
