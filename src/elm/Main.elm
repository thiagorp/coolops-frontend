module Main exposing (main)

import App.Main as App
import Auth.Main as Auth
import Html
import Navigation
import Ports exposing (onSessionChange)
import Route


type Msg
    = UrlChanged Navigation.Location
    | SessionChanged (Maybe String)
    | AppMsg App.Msg
    | AuthMsg Auth.Msg


type Page
    = Transitioning
    | App App.Model
    | Auth Auth.Model


type alias Flags =
    { token : Maybe String
    , baseUrl : String
    }


type alias Model =
    { page : Page
    , token : Maybe String
    , baseUrl : String
    }


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
        handleAuthRoute =
            Route.isProtectedRoute location
                |> handleRoute model Route.authRoot

        handleProtectedRoute =
            Route.isAuthRoute location
                |> handleRoute model Route.protectedRoot
    in
    case ( model.page, model.token ) of
        ( Transitioning, Just token ) ->
            App.init model.baseUrl token location
                |> wrapPage App AppMsg model
                |> handleProtectedRoute

        ( Transitioning, Nothing ) ->
            Auth.init model.baseUrl location
                |> wrapPage Auth AuthMsg model
                |> handleAuthRoute

        ( App subModel, Just _ ) ->
            App.update (App.UrlChanged location) subModel
                |> wrapPage App AppMsg model
                |> handleProtectedRoute

        ( App _, Nothing ) ->
            model
                |> redirectTo Route.authRoot

        ( Auth subModel, Nothing ) ->
            Auth.update (Auth.UrlChanged location) subModel
                |> wrapPage Auth AuthMsg model
                |> handleAuthRoute

        ( Auth _, Just _ ) ->
            model
                |> redirectTo Route.protectedRoot


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init { token, baseUrl } =
    setPage (Model Transitioning token baseUrl)


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
                            Route.authRoot
            in
            ( { model | token = newToken }, Route.redirectTo page )

        ( UrlChanged location, _ ) ->
            setPage model location

        ( AuthMsg subMsg, Auth subModel ) ->
            Auth.update subMsg subModel
                |> wrapPage Auth AuthMsg model

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

        Auth subModel ->
            Auth.view subModel
                |> Html.map AuthMsg


pageSubscriptions : Model -> Sub Msg
pageSubscriptions model =
    case model.page of
        Transitioning ->
            Sub.none

        App subModel ->
            App.subscriptions subModel
                |> Sub.map AppMsg

        Auth subModel ->
            Auth.subscriptions subModel
                |> Sub.map AuthMsg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onSessionChange SessionChanged
        , pageSubscriptions model
        ]


main : Program Flags Model Msg
main =
    Navigation.programWithFlags UrlChanged
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
