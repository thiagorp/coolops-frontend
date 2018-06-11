module App.Main exposing (..)

import App.Fragments.Topbar as Topbar
import App.Pages.NotFound as NotFound
import App.Pages.ProjectsList as ProjectsList
import Html
import Navigation
import Route
import Util


type Msg
    = UrlChanged Navigation.Location
    | ProjectsListMsg ProjectsList.Msg
    | TopbarMsg Topbar.Msg


type Content
    = ProjectsList ProjectsList.Model


type Page
    = NotFound
    | App Topbar.Model Content


type alias Model =
    { page : Page }


wrapTopbar : Model -> Content -> Cmd Msg -> ( Topbar.Model, List (Cmd Topbar.Msg) ) -> ( Model, Cmd Msg )
wrapTopbar model content cmds ( subModel, subCmds ) =
    let
        cmd =
            Util.processCmds subCmds
                |> Cmd.map TopbarMsg
    in
    ( { model | page = App subModel content }, Cmd.batch [ cmd, cmds ] )


wrapPage : (model -> Content) -> (msg -> Msg) -> Model -> ( model, List (Cmd msg) ) -> ( Model, Cmd Msg )
wrapPage toContent toMsg model ( subModel, subCmds ) =
    let
        cmd =
            Util.processCmds subCmds
                |> Cmd.map toMsg

        content =
            toContent subModel
    in
    case model.page of
        App topbar _ ->
            ( { model | page = App topbar content }, cmd )

        NotFound ->
            Topbar.init
                |> wrapTopbar model content cmd


setPage : Model -> Navigation.Location -> ( Model, Cmd Msg )
setPage model location =
    case Route.readProtectedRoute location of
        Nothing ->
            ( { page = NotFound }, Cmd.none )

        Just Route.ProjectsList ->
            wrapPage ProjectsList ProjectsListMsg model ProjectsList.init


init : Navigation.Location -> ( Model, Cmd Msg )
init =
    setPage (Model NotFound)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged location ->
            setPage model location

        ProjectsListMsg _ ->
            ( model, Cmd.none )

        TopbarMsg subMsg ->
            case model.page of
                App subModel content ->
                    Topbar.update subMsg subModel
                        |> wrapTopbar model content Cmd.none

                NotFound ->
                    ( model, Cmd.none )


inLayout : Topbar.Model -> Html.Html Msg -> Html.Html Msg
inLayout tobarModel page =
    Html.div []
        [ tobarModel |> Topbar.view |> Html.map TopbarMsg
        , page
        ]


contentView : Content -> Html.Html Msg
contentView content =
    case content of
        ProjectsList subModel ->
            ProjectsList.view subModel
                |> Html.map ProjectsListMsg


view : Model -> Html.Html Msg
view model =
    case model.page of
        NotFound ->
            NotFound.view

        App tobar content ->
            contentView content
                |> inLayout tobar


main : Program Never Model Msg
main =
    Navigation.program UrlChanged
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
