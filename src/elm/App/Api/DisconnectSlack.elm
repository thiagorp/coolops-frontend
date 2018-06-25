module App.Api.DisconnectSlack exposing (..)

import App.Api.Common exposing (..)
import Http


disconnectSlack : Token -> (Result Http.Error () -> msg) -> Cmd msg
disconnectSlack token msg =
    delete token "/slack_config"
        |> Http.send msg
