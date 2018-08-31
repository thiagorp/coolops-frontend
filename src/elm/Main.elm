module Main exposing (main)

import App.Main as App
import Auth.Main as Auth
import Html
import Navigation
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


handleAppLocation : Model -> Navigation.Location -> ( Model, Cmd Msg )
handleAppLocation model location =
    case model.token of
        Nothing ->
            model
                |> redirectTo Route.authRoot

        Just token ->
            case model.page of
                App subModel ->
                    App.update (App.UrlChanged location) subModel
                        |> wrapPage App AppMsg model

                _ ->
                    App.init model.baseUrl token location
                        |> wrapPage App AppMsg model


handleAuthLocation : Model -> Navigation.Location -> ( Model, Cmd Msg )
handleAuthLocation model location =
    case model.token of
        Just _ ->
            model
                |> redirectTo Route.protectedRoot

        Nothing ->
            case model.page of
                Auth subModel ->
                    Auth.update (Auth.UrlChanged location) subModel
                        |> wrapPage Auth AuthMsg model

                _ ->
                    Auth.init model.baseUrl location
                        |> wrapPage Auth AuthMsg model


setPage : Model -> Navigation.Location -> ( Model, Cmd Msg )
setPage model location =
    case Route.readPublicRoute location of
        Nothing ->
            case Route.isProtectedRoute location of
                False ->
                    handleAuthLocation model location

                True ->
                    handleAppLocation model location

        Just publicPage ->
            case model.page of
                Public subModel ->
                    Public.update (Public.UrlChanged publicPage) subModel
                        |> wrapPage Public PublicMsg model

                _ ->
                    Public.init model.baseUrl publicPage
                        |> wrapPage Public PublicMsg model


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
