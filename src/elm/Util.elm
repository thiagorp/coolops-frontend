module Util exposing (..)


type alias PageHandler model msg =
    ( model, List (Cmd msg) )


return : model -> PageHandler model msg
return model =
    ( model, [] )


noop : model -> PageHandler model msg
noop =
    return


processCmds : List (Cmd msg) -> Cmd msg
processCmds cmds =
    case cmds of
        [] ->
            Cmd.none

        list ->
            Cmd.batch list


andPerform : Cmd msg -> PageHandler model msg -> PageHandler model msg
andPerform newCommand ( model, commands ) =
    ( model, newCommand :: commands )
