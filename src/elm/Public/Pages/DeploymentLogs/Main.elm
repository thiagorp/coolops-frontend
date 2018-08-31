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
import Time exposing (Time, second)
import Util exposing (PageHandler, andPerform, noop, return)


type Msg
    = DeploymentLogsResponse (Result Error DeploymentLogs)
    | Tick Time


type alias Model =
    { baseUrl : String, deploymentId : String, logs : WebData DeploymentLogs, spinnerState : Bool }


init : String -> String -> PageHandler Model Msg
init baseUrl deploymentId =
    return { baseUrl = baseUrl, deploymentId = deploymentId, logs = Loading, spinnerState = False }
        |> andPerform (getDeploymentLogs baseUrl deploymentId DeploymentLogsResponse)


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        DeploymentLogsResponse result ->
            { model | logs = RemoteData.fromResult result }
                |> return

        Tick _ ->
            return { model | spinnerState = not model.spinnerState }
                |> andPerform (getDeploymentLogs model.baseUrl model.deploymentId DeploymentLogsResponse)


logsView : DeploymentLogs -> Html Msg
logsView { logs } =
    pre
        [ style
            [ ( "backgroundColor", "#212121" )
            , ( "color", "#edede3" )
            , ( "marginTop", "30px" )
            ]
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
                [ style
                    [ ( "width", "20px" )
                    , ( "height", "20px" )
                    , ( "borderRadius", "10px" )
                    , ( "backgroundColor", "#edede3" )
                    , ( "transform", transform )
                    , ( "transition", "all 1s ease-in-out" )
                    ]
                ]
                []


view : Model -> Html Msg
view model =
    case model.logs of
        Success deploymentLogs ->
            div [ class "container" ]
                [ div [ style [ ( "position", "relative" ) ] ]
                    [ div [ style [ ( "position", "absolute" ), ( "bottom", "10px" ), ( "right", "10px" ) ] ]
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
                    Time.every second Tick

                True ->
                    Sub.none

        _ ->
            Sub.none
