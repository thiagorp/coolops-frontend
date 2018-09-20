module Public.Pages.DeploymentLogs.Main exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import Public.Pages.DeploymentLogs.Data exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Time exposing (Posix)
import Util exposing (PageHandler, andPerform, noop, return)


type Msg
    = DeploymentLogsResponse (Result Error DeploymentLogs)
    | Tick Time.Posix


type alias Model =
    { baseUrl : String
    , deploymentId : String
    , logs : WebData DeploymentLogs
    , spinnerState : Bool
    , loadingData : Bool
    }



-- Init


init : String -> String -> PageHandler Model Msg
init baseUrl deploymentId =
    return { baseUrl = baseUrl, deploymentId = deploymentId, logs = Loading, spinnerState = False, loadingData = True }
        |> andPerform (getDeploymentLogs baseUrl deploymentId DeploymentLogsResponse)



-- Update


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        DeploymentLogsResponse result ->
            { model | logs = RemoteData.fromResult result, loadingData = False }
                |> return

        Tick _ ->
            case model.loadingData of
                True ->
                    return { model | spinnerState = not model.spinnerState }

                False ->
                    return { model | spinnerState = not model.spinnerState, loadingData = True }
                        |> andPerform (getDeploymentLogs model.baseUrl model.deploymentId DeploymentLogsResponse)



-- View


logsView : DeploymentLogs -> Html Msg
logsView { logs } =
    pre
        [ style "backgroundColor" "#212121"
        , style "color" "#edede3"
        , style "marginTop" "30px"
        ]
        [ text logs ]


spinner : DeploymentLogs -> Bool -> Html Msg
spinner { finished } state =
    let
        transform =
            case state of
                True ->
                    "scale(0)"

                False ->
                    "scale(1)"
    in
    case finished of
        True ->
            div [] []

        False ->
            div
                [ style "width" "20px"
                , style "height" "20px"
                , style "borderRadius" "10px"
                , style "backgroundColor" "#edede3"
                , style "transform" transform
                , style "transition" "all 1s ease-in-out"
                ]
                []


view : Model -> Html Msg
view model =
    case model.logs of
        Success deploymentLogs ->
            div [ class "container" ]
                [ div [ style "position" "relative" ]
                    [ div [ style "position" "absolute", style "bottom" "10px", style "right" "10px" ]
                        [ spinner deploymentLogs model.spinnerState ]
                    , logsView deploymentLogs
                    ]
                ]

        _ ->
            div [] []


subscriptions : Model -> Sub Msg
subscriptions { logs } =
    case logs of
        Success deploymentLogs ->
            case deploymentLogs.finished of
                False ->
                    Time.every 1000 Tick

                True ->
                    Sub.none

        _ ->
            Sub.none
