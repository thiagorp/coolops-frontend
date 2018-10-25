module App.Forms.ProjectSlackIntegrations.SelectChannel exposing
    ( Model
    , Msg(..)
    , init
    , isSubmitting
    , update
    , view
    )

import Api
import App.Api.CreateSlackProjectIntegration as Api
import App.Html.Form as Form
import Form.Validation as Validation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Route
import SelectList exposing (SelectList)
import Util exposing (PageHandler, andPerform, noop, return)



-- Model


type alias SlackAccessToken a =
    { a | channels : List SlackChannel }


type alias SlackChannel =
    { key : String
    , val : String
    }


type alias SlackIntegration a =
    { a | channelId : String }


type alias Project a b =
    { a
        | id : String
        , slackIntegration : Maybe (SlackIntegration b)
    }


type alias Model =
    { apiConfig : Api.ProtectedConfig
    , channels : SelectList { key : String, val : String }
    , formState : Validation.FormState Field
    , projectId : String
    }



-- Msg


type Msg
    = SlackChannelSelected String
    | Submit
    | SubmitResponse (Result Http.Error ())



-- Init


channelsFrom : Project a b -> SlackAccessToken c -> SelectList { key : String, val : String }
channelsFrom project { channels } =
    let
        selectedId =
            project.slackIntegration
                |> Maybe.map .channelId
                |> Maybe.withDefault ""

        sortedChannels =
            List.sortBy .val channels
    in
    { key = "", val = "" }
        |> SelectList.singleton
        |> SelectList.append sortedChannels
        |> SelectList.select ((==) selectedId << .key)


init : Api.ProtectedConfig -> SlackAccessToken a -> Project b c -> PageHandler Model Msg
init apiConfig slackAccessToken project =
    return
        { apiConfig = apiConfig
        , channels = channelsFrom project slackAccessToken
        , formState = Validation.initialState
        , projectId = project.id
        }



-- Update


type Field
    = ChannelsField


submit : Model -> PageHandler Model Msg
submit model =
    let
        params =
            model.channels
                |> SelectList.selected
                |> (\{ key, val } -> { channelName = val, channelId = key })
    in
    return model
        |> andPerform
            (Api.createSlackProjectIntegration model.apiConfig model.projectId SubmitResponse params)


formConfig : Validation.FormConfig Model Field (PageHandler Model Msg)
formConfig =
    let
        validator =
            Validation.all
                [ Validation.ifBlank (.key << SelectList.selected << .channels) ChannelsField
                ]
    in
    { validator = validator
    , successCallback = submit
    , errorCallback = noop
    }


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        SlackChannelSelected channel ->
            { model | channels = SelectList.select ((==) channel << .key) model.channels }
                |> Validation.validate formConfig
                |> return

        Submit ->
            Validation.submit formConfig model

        SubmitResponse (Ok _) ->
            return { model | formState = Validation.initialState }

        SubmitResponse (Err _) ->
            Validation.serverError model
                |> return



-- View


isSubmitting : Model -> Bool
isSubmitting model =
    Validation.isSubmitting model.formState


view : Model -> List (Html Msg)
view model =
    let
        inputs =
            [ Form.DropdownInput
                { label = "Which channel would you like to receive messages about this project?"
                , options = model.channels
                , errors = Validation.errorsOf model.formState ChannelsField
                , disabled = isSubmitting model
                , hint = Just [ text "You might need to add the ", b [] [ text "coolops.io" ], text " bot to the channel if you can't find it here." ]
                , events =
                    { onSelect = SlackChannelSelected
                    }
                , id = "new-project-slack-integration-slack-channel"
                }
            ]

        serverError =
            case Validation.getServerError model.formState of
                Nothing ->
                    []

                Just error ->
                    [ p [ class "text-red" ] [ text error ] ]
    in
    List.map Form.input inputs
        ++ serverError
