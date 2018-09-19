module Util.Slug exposing (isValid, slugify)

import Regex exposing (..)


replaceRegex : List ( Maybe Regex, String )
replaceRegex =
    [ ( fromString "[^a-z0-9 \\-]", "" )
    , ( fromString "[áàãâ]", "a" )
    , ( fromString "[éêèëēėę]", "e" )
    , ( fromString "[îïíīįì]", "i" )
    , ( fromString "[ôòóøōõ]", "o" )
    , ( fromString "[ûùúū]", "u" )
    , ( fromString "[ç]", "c" )
    , ( fromString "ß", "ss" )
    , ( fromString "ä", "ae" )
    , ( fromString "ö", "oe" )
    , ( fromString "ü", "ue" )
    ]


replaceMaybe : Maybe Regex -> (Match -> String) -> String -> String
replaceMaybe maybeRegex replacer word =
    case maybeRegex of
        Nothing ->
            word

        Just regex ->
            replace regex replacer word


replaceInvalid : String -> String
replaceInvalid string =
    List.foldr (\( reg, val ) str -> replaceMaybe reg (\_ -> val) str) string replaceRegex


isValid : String -> Bool
isValid string =
    case fromString "^[a-z0-9\\-]*$" of
        Just regex ->
            contains regex string

        Nothing ->
            False


slugify : String -> String
slugify string =
    string
        |> String.toLower
        |> replaceInvalid
        |> String.words
        |> String.join "-"
