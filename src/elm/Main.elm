module Main exposing (main)

import App.Main as App
import Auth.Main as Auth
import Html
import Navigation
import NotFound
import Ports exposing (onSessionChange)
import Public.Main as Public
import Route


type Msg
    = UrlChanged Navigation.Location
    | SessionChanged (Maybe String)
    | AppMsg App.Msg
    | AuthMsg Auth.Msg
    | PublicMsg Public.Msg


type Page
    = Transitioning
    | App App.Model
    | Auth Auth.Model
    | Public Public.Model
    | NotFound


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


redirectTo : Route.Route -> Model -> ( Model, Cmd Msg )
redirectTo route model =
    ( { model | page = Transitioning }, Route.redirectTo route )


handleAppRoute : Model -> Route.ProtectedRoute -> ( Model, Cmd Msg )
handleAppRoute model route =
    case model.token of
        Nothing ->
            model
                |> redirectTo Route.authRoot

        Just token ->
            case model.page of
                App subModel ->
                    App.update (App.UrlChanged route) subModel
                        |> wrapPage App AppMsg model

                _ ->
                    App.init model.baseUrl token route
                        |> wrapPage App AppMsg model


handleAuthRoute : Model -> Route.AuthRoute -> ( Model, Cmd Msg )
handleAuthRoute model route =
    case model.token of
        Just _ ->
            model
                |> redirectTo Route.protectedRoot

        Nothing ->
            case model.page of
                Auth subModel ->
                    Auth.update (Auth.UrlChanged route) subModel
                        |> wrapPage Auth AuthMsg model

                _ ->
                    Auth.init model.baseUrl route
                        |> wrapPage Auth AuthMsg model


handlePublicRoute : Model -> Route.PublicRoute -> ( Model, Cmd Msg )
handlePublicRoute model route =
    case model.page of
        Public subModel ->
            Public.update (Public.UrlChanged route) subModel
                |> wrapPage Public PublicMsg model

        _ ->
            Public.init model.baseUrl route
                |> wrapPage Public PublicMsg model


setPage : Model -> Navigation.Location -> ( Model, Cmd Msg )
setPage model location =
    case Route.routeFromLocation location of
        Just (Route.Protected route) ->
            handleAppRoute model route

        Just (Route.Auth route) ->
            handleAuthRoute model route

        Just (Route.Public route) ->
            handlePublicRoute model route

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init { token, baseUrl } =
    setPage (Model Transitioning token baseUrl)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SessionChanged newToken ->
            let
                page =
                    case newToken of
                        Just _ ->
                            Route.protectedRoot

                        Nothing ->
                            Route.authRoot
            in
            ( { model | token = newToken }, Route.redirectTo page )

        UrlChanged location ->
            setPage model location

        AuthMsg subMsg ->
            case model.page of
                Auth subModel ->
                    Auth.update subMsg subModel
                        |> wrapPage Auth AuthMsg model

                _ ->
                    ( model, Cmd.none )

        AppMsg subMsg ->
            case model.page of
                App subModel ->
                    App.update subMsg subModel
                        |> wrapPage App AppMsg model

                _ ->
                    ( model, Cmd.none )

        PublicMsg subMsg ->
            case model.page of
                Public subModel ->
                    Public.update subMsg subModel
                        |> wrapPage Public PublicMsg model

                _ ->
                    ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    case model.page of
        Transitioning ->
            Html.p [] [ Html.text "Redirecting..." ]

        NotFound ->
            NotFound.view

        App subModel ->
            App.view subModel
                |> Html.map AppMsg

        Auth subModel ->
            Auth.view subModel
                |> Html.map AuthMsg

        Public subModel ->
            Public.view subModel
                |> Html.map PublicMsg


pageSubscriptions : Model -> Sub Msg
pageSubscriptions model =
    case model.page of
        Transitioning ->
            Sub.none

        NotFound ->
            Sub.none

        App subModel ->
            App.subscriptions subModel
                |> Sub.map AppMsg

        Auth subModel ->
            Auth.subscriptions subModel
                |> Sub.map AuthMsg

        Public subModel ->
            Public.subscriptions subModel
                |> Sub.map PublicMsg


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
