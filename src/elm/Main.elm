module Main exposing (..)

import App.Main as App
import Html
import Navigation
import Ports exposing (onSessionChange)
import Public.Main as Public
import Route


type Msg
    = UrlChanged Navigation.Location
    | SessionChanged (Maybe String)
    | AppMsg App.Msg
    | PublicMsg Public.Msg


type Page
    = Transitioning
    | App App.Model
    | Public Public.Model


type alias Flags =
    { token : Maybe String }


type alias Model =
    { page : Page, token : Maybe String }


wrapPage : (model -> Page) -> (msg -> Msg) -> Model -> ( model, Cmd msg ) -> ( Model, Cmd Msg )
wrapPage toPage toCmd model ( subModel, subCmd ) =
    ( { model | page = toPage subModel }, Cmd.map toCmd subCmd )


handleRoute : Model -> Route.Route -> Bool -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleRoute model route redirect return =
    case redirect of
        True ->
            ( { model | page = Transitioning }, Route.redirectTo route )

        False ->
            return


redirectTo : Route.Route -> Model -> ( Model, Cmd Msg )
redirectTo route model =
    ( { model | page = Transitioning }, Route.redirectTo route )


setPage : Model -> Navigation.Location -> ( Model, Cmd Msg )
setPage model location =
    let
        handleOpenRoute =
            Route.isProtectedRoute location
                |> handleRoute model Route.openRoot

        handleProtectedRoute =
            Route.isOpenRoute location
                |> handleRoute model Route.protectedRoot
    in
    case ( model.page, model.token ) of
        ( Transitioning, Just token ) ->
            App.init token location
                |> wrapPage App AppMsg model
                |> handleProtectedRoute

        ( Transitioning, Nothing ) ->
            Public.init location
                |> wrapPage Public PublicMsg model
                |> handleOpenRoute

        ( App subModel, Just _ ) ->
            App.update (App.UrlChanged location) subModel
                |> wrapPage App AppMsg model
                |> handleProtectedRoute

        ( App _, Nothing ) ->
            model
                |> redirectTo Route.openRoot

        ( Public subModel, Nothing ) ->
            Public.update (Public.UrlChanged location) subModel
                |> wrapPage Public PublicMsg model
                |> handleOpenRoute

        ( Public _, Just _ ) ->
            model
                |> redirectTo Route.protectedRoot


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init { token } =
    setPage (Model Transitioning token)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( SessionChanged newToken, _ ) ->
            let
                page =
                    case newToken of
                        Just _ ->
                            Route.protectedRoot

                        Nothing ->
                            Route.openRoot
            in
            ( { model | token = newToken }, Route.redirectTo page )

        ( UrlChanged location, _ ) ->
            setPage model location

        ( PublicMsg subMsg, Public subModel ) ->
            Public.update subMsg subModel
                |> wrapPage Public PublicMsg model

        ( AppMsg subMsg, App subModel ) ->
            App.update subMsg subModel
                |> wrapPage App AppMsg model

        _ ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    case model.page of
        Transitioning ->
            Html.p [] [ Html.text "Redirecting..." ]

        App subModel ->
            App.view subModel
                |> Html.map AppMsg

        Public subModel ->
            Public.view subModel
                |> Html.map PublicMsg


subscriptions : Model -> Sub Msg
subscriptions model =
    onSessionChange SessionChanged


main : Program Flags Model Msg
main =
    Navigation.programWithFlags UrlChanged
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
