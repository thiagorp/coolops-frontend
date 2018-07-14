module App.Api.ListProjects exposing (..)

import App.Api.Common exposing (..)
import Date exposing (..)
import Http
import Json.Decode as Decode
import Time exposing (..)


type DeploymentStatus
    = Queued
    | Running
    | Succeeded
    | Failed


type alias Build =
    { id : String
    , name : String
    }


type alias Deployment =
    { id : String
    , build : Build
    , startedAt : Time
    , status : DeploymentStatus
    }


type alias Environment =
    { id : String
    , name : String
    , currentDeployment : Maybe Deployment
    }


type alias Project =
    { id : String
    , name : String
    , deploymentImage : String
    , environments : List Environment
    }


timeDecoder : Decode.Decoder Time
timeDecoder =
    Decode.string
        |> Decode.andThen
            (\dateString ->
                case Date.fromString dateString of
                    Ok date ->
                        Decode.succeed <| Date.toTime date

                    Err errorString ->
                        Decode.fail errorString
            )


statusDecoder : Decode.Decoder DeploymentStatus
statusDecoder =
    Decode.string
        |> Decode.andThen
            (\statusString ->
                case statusString of
                    "running" ->
                        Decode.succeed Running

                    "queued" ->
                        Decode.succeed Queued

                    "failed" ->
                        Decode.succeed Failed

                    "succeeded" ->
                        Decode.succeed Succeeded

                    _ ->
                        Decode.fail "Unknown status"
            )


buildDecoder : Decode.Decoder Build
buildDecoder =
    Decode.map2 Build
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)


deploymentDecoder : Decode.Decoder Deployment
deploymentDecoder =
    Decode.map4 Deployment
        (Decode.field "id" Decode.string)
        (Decode.field "build" buildDecoder)
        (Decode.field "last_changed_at" timeDecoder)
        (Decode.field "status" statusDecoder)


environmentDecoder : Decode.Decoder Environment
environmentDecoder =
    Decode.map3 Environment
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "current_deployment" (Decode.maybe deploymentDecoder))


decoder : Decode.Decoder Project
decoder =
    Decode.map4 Project
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "deployment_image" Decode.string)
        (Decode.field "environments" (Decode.list environmentDecoder))


listProjects : String -> Token -> (Result Http.Error (List Project) -> msg) -> Cmd msg
listProjects baseUrl token msg =
    get baseUrl token "/projects" (Http.expectJson (Decode.list decoder))
        |> Http.send msg
