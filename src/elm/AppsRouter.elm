module AppsRouter exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

import Api
import App.Main as App
import App.Pages.ServerError
import Auth.Main as Auth
import Browser
import Browser.Navigation as Navigation
import Html
import Json.Decode as Decode
import NotFound
import Ports exposing (onSessionChange)
import Public.Main as Public
import Route
import Url


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | SessionChanged Decode.Value
    | AppMsg App.Msg
    | AuthMsg Auth.Msg
    | PublicMsg Public.Msg


type Page
    = Transitioning
    | App App.Model
    | Auth Auth.Model
    | Public Public.Model
    | NotFound


type alias Model =
    { page : Page
    , token : Maybe Api.Token
    , key : Navigation.Key
    , baseUrl : Api.BaseUrl
    }


wrapPage : (model -> Page) -> (msg -> Msg) -> Model -> ( model, Cmd msg ) -> ( Model, Cmd Msg )
wrapPage toPage toCmd model ( subModel, subCmd ) =
    ( { model | page = toPage subModel }, Cmd.map toCmd subCmd )


redirectTo : Route.Route -> Model -> ( Model, Cmd Msg )
redirectTo route model =
    ( { model | page = Transitioning }, Route.redirectTo model.key route )


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
                    App.init { baseUrl = model.baseUrl, token = token } model.key route
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
            Public.init model.baseUrl model.key route
                |> wrapPage Public PublicMsg model


setPage : Model -> Url.Url -> ( Model, Cmd Msg )
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


type alias Flags =
    { token : Maybe Api.Token
    , baseUrl : Api.BaseUrl
    }


init : Maybe Api.Token -> Api.BaseUrl -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init token baseUrl url navigationKey =
    setPage (Model Transitioning token navigationKey baseUrl) url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SessionChanged value ->
            let
                decoderResult =
                    Decode.decodeValue (Decode.nullable Api.tokenDecoder) value

                ( page, newToken ) =
                    case decoderResult of
                        Err _ ->
                            ( Route.authRoot, Nothing )

                        Ok (Just token) ->
                            ( Route.protectedRoot, Just token )

                        Ok Nothing ->
                            ( Route.authRoot, Nothing )
            in
            ( { model | token = newToken }, Route.redirectTo model.key page )

        UrlChanged location ->
            setPage model location

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Navigation.load href )

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


view : Model -> Browser.Document Msg
view model =
    let
        body =
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
    in
    { title = "coolops.io", body = [ body ] }


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
