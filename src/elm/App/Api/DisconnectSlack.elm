module App.Api.DisconnectSlack exposing (..)

import App.Api.Common exposing (..)
import Http


disconnectSlack : String -> Token -> (Result Http.Error () -> msg) -> Cmd msg
disconnectSlack baseUrl token msg =
    delete baseUrl token "/slack_config"
        |> Http.send msg
