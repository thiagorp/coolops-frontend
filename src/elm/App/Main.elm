module App.Main exposing (..)

import App.Pages.NotFound as NotFound
import App.Pages.ProjectsList as ProjectsList
import Html
import Navigation
import Route


type Msg
    = UrlChanged Navigation.Location
    | ProjectsListMsg ProjectsList.Msg


type Page
    = NotFound
    | ProjectsList ProjectsList.Model


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
    case ( msg, model.page ) of
        ( UrlChanged location, _ ) ->
            setPage model location

        _ ->
            ( model, Cmd.none )


inLayout : Html.Html Msg -> Html.Html Msg
inLayout page =
    Html.div []
        [ Html.h1 [] [ Html.text "Layout" ]
        , page
        ]


view : Model -> Html.Html Msg
view model =
    case model.page of
        NotFound ->
            NotFound.view

        ProjectsList subModel ->
            ProjectsList.view subModel
                |> Html.map ProjectsListMsg
                |> inLayout


main : Program Never Model Msg
main =
    Navigation.program UrlChanged
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
