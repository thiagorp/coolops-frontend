module App.Pages.Projects.Edit.SlackSection exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Api
import App.Forms.ProjectSlackIntegrations.Authorize as Authorize
import App.Forms.ProjectSlackIntegrations.SelectChannel as SelectChannel
import App.Html.Form as Form
import App.Pages.Projects.Edit.Data as Data
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Route
import Time
import Util exposing (PageHandler, andPerform_, noop, return)



-- Model


type Page
    = Authorize Authorize.Model
    | SelectChannel SelectChannel.Model


type alias Model =
    { page : Page
    , showSuccessMessage : Bool
    }



-- Msg


type Msg
    = AuthorizeMsg Authorize.Msg
    | DismissSuccessMessage
    | SelectChannelMsg SelectChannel.Msg



-- Init


init :
    Api.ProtectedConfig
    -> Route.NavigationKey
    -> Data.Project
    -> Maybe Data.SlackAccessToken
    -> Data.SlackConfiguration
    -> Bool
    -> PageHandler Model Msg
init apiConfig navigationKey project maybeAccessToken slackConfig error =
    let
        ( page, cmd ) =
            case maybeAccessToken of
                Just accessToken ->
                    SelectChannel.init apiConfig accessToken project
                        |> Util.map SelectChannel SelectChannelMsg

                Nothing ->
                    Authorize.init navigationKey slackConfig project error ""
                        |> Util.map Authorize AuthorizeMsg
    in
    return { page = page, showSuccessMessage = False }
        |> andPerform_ cmd



-- Update


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        AuthorizeMsg subMsg ->
            case model.page of
                Authorize subModel ->
                    Authorize.update subMsg subModel
                        |> Util.map (\m -> { model | page = Authorize m }) AuthorizeMsg

                _ ->
                    return model

        DismissSuccessMessage ->
            return { model | showSuccessMessage = False }

        SelectChannelMsg subMsg ->
            case model.page of
                SelectChannel subModel ->
                    case subMsg of
                        SelectChannel.SubmitResponse (Ok _) ->
                            SelectChannel.update subMsg subModel
                                |> Util.map (\m -> { model | page = SelectChannel m, showSuccessMessage = True }) SelectChannelMsg

                        SelectChannel.Submit ->
                            SelectChannel.update subMsg subModel
                                |> Util.map (\m -> { model | page = SelectChannel m, showSuccessMessage = False }) SelectChannelMsg

                        _ ->
                            SelectChannel.update subMsg subModel
                                |> Util.map (\m -> { model | page = SelectChannel m }) SelectChannelMsg

                _ ->
                    return model



-- View


view : Model -> Html Msg
view model =
    case model.page of
        Authorize subModel ->
            div [ class "card" ]
                [ div [ class "card-header" ]
                    [ h3 [ class "card-title" ] [ text "Slack integration" ]
                    ]
                , div [ class "card-body" ] (Authorize.view subModel)
                ]
                |> Html.map AuthorizeMsg

        SelectChannel subModel ->
            div [ class "card" ]
                [ Html.form [ onSubmit SelectChannel.Submit ]
                    [ div [ class "card-header" ]
                        [ h3 [ class "card-title" ] [ text "Slack integration" ]
                        ]
                    , div [ class "card-body" ] (SelectChannel.view subModel)
                    , div [ class "card-footer text-right" ]
                        [ span [ class "text-success mr-4 small font-weight-light", classList [ ( "d-none", not model.showSuccessMessage ) ] ] [ text "Saved with success" ]
                        , Form.submitButton "Save" (SelectChannel.isSubmitting subModel)
                        ]
                    ]
                ]
                |> Html.map SelectChannelMsg



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.showSuccessMessage of
        True ->
            Time.every (5 * 1000) (\_ -> DismissSuccessMessage)

        False ->
            Sub.none
