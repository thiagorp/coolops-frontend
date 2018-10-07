module Main exposing (main)

import Api
import App.Pages.ServerError as ServerError
import AppsRouter
import Browser
import Browser.Navigation as Navigation
import Html
import Json.Decode as Decode
import Url


type Msg
    = AppsRouterMsg AppsRouter.Msg
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest


type Page
    = AppsRouter AppsRouter.Model
    | ServerError


type alias Model =
    { page : Page
    , key : Navigation.Key
    }


type alias Flags =
    { token : Maybe Api.Token
    , baseUrl : Api.BaseUrl
    }


init : Decode.Value -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url navigationKey =
    let
        flagsDecoder =
            Decode.map2 Flags
                (Decode.field "token" (Decode.nullable Api.tokenDecoder))
                (Decode.field "baseUrl" Api.baseUrlDecoder)
    in
    case Decode.decodeValue flagsDecoder flags of
        Ok { token, baseUrl } ->
            let
                ( model, cmd ) =
                    AppsRouter.init token baseUrl url navigationKey
            in
            ( Model (AppsRouter model) navigationKey, cmd |> Cmd.map AppsRouterMsg )

        Err _ ->
            ( Model ServerError navigationKey, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AppsRouterMsg subMsg ->
            case model.page of
                AppsRouter subModel ->
                    let
                        ( newSubModel, cmd ) =
                            AppsRouter.update subMsg subModel
                    in
                    ( { model | page = AppsRouter newSubModel }, cmd |> Cmd.map AppsRouterMsg )

                _ ->
                    ( model, Cmd.none )

        UrlChanged url ->
            case model.page of
                AppsRouter subModel ->
                    let
                        ( newSubModel, cmd ) =
                            AppsRouter.update (AppsRouter.UrlChanged url) subModel
                    in
                    ( { model | page = AppsRouter newSubModel }, cmd |> Cmd.map AppsRouterMsg )

                _ ->
                    ( model, Cmd.none )

        LinkClicked urlRequest ->
            case model.page of
                AppsRouter subModel ->
                    let
                        ( newSubModel, cmd ) =
                            AppsRouter.update (AppsRouter.LinkClicked urlRequest) subModel
                    in
                    ( { model | page = AppsRouter newSubModel }, cmd |> Cmd.map AppsRouterMsg )

                _ ->
                    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    case model.page of
        AppsRouter subModel ->
            let
                { title, body } =
                    AppsRouter.view subModel
            in
            { title = title, body = List.map (Html.map AppsRouterMsg) body }

        ServerError ->
            { title = "coolops.io", body = [ ServerError.view ] }


pageSubscriptions : Model -> Sub Msg
pageSubscriptions model =
    case model.page of
        AppsRouter subModel ->
            AppsRouter.subscriptions subModel
                |> Sub.map AppsRouterMsg

        ServerError ->
            Sub.none


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ pageSubscriptions model
        ]


main : Program Decode.Value Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
