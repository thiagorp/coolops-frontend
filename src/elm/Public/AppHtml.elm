module Public.AppHtml exposing (a)

import Html exposing (Attribute, Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Route exposing (Route, toUrl)


a : Route -> (Route -> msg) -> List (Attribute msg) -> List (Html msg) -> Html msg
a route msg customAttributes children =
    let
        attributes =
            customAttributes
                ++ [ href (toUrl route), onPreventDefaultClick (msg route) ]
    in
    Html.a attributes children



-- See https://github.com/elm-lang/html/issues/110


onPreventDefaultClick : msg -> Attribute msg
onPreventDefaultClick message =
    onWithOptions "click"
        { defaultOptions | preventDefault = True }
        (preventDefault2
            |> Json.Decode.andThen (maybePreventDefault message)
        )


preventDefault2 : Json.Decode.Decoder Bool
preventDefault2 =
    Json.Decode.map2
        invertedOr
        (Json.Decode.field "ctrlKey" Json.Decode.bool)
        (Json.Decode.field "metaKey" Json.Decode.bool)


maybePreventDefault : msg -> Bool -> Json.Decode.Decoder msg
maybePreventDefault msg preventDefault =
    case preventDefault of
        True ->
            Json.Decode.succeed msg

        False ->
            Json.Decode.fail "Normal link"


invertedOr : Bool -> Bool -> Bool
invertedOr x y =
    not (x || y)



-- End https://github.com/elm-lang/html/issues/110
