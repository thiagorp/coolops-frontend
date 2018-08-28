module Util exposing (PageHandler, andPerform, map, noop, processCmds, return)


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


map : (subModel -> model) -> (subMsg -> msg) -> PageHandler subModel subMsg -> PageHandler model msg
map modelFn msgFn ( model, cmds ) =
    ( modelFn model, List.map (Cmd.map msgFn) cmds )
