module Util.Slug exposing (isValid, slugify)

import Regex exposing (..)


replaceRegex : List ( Regex, String )
replaceRegex =
    [ ( regex "[^a-z0-9 \\-]", "" )
    , ( regex "[áàãâ]", "a" )
    , ( regex "[éêèëēėę]", "e" )
    , ( regex "[îïíīįì]", "i" )
    , ( regex "[ôòóøōõ]", "o" )
    , ( regex "[ûùúū]", "u" )
    , ( regex "[ç]", "c" )
    , ( regex "ß", "ss" )
    , ( regex "ä", "ae" )
    , ( regex "ö", "oe" )
    , ( regex "ü", "ue" )
    ]


replaceInvalid : String -> String
replaceInvalid string =
    List.foldr (\( reg, val ) str -> replace All reg (\_ -> val) str) string replaceRegex


isValid : String -> Bool
isValid =
    contains (regex "^[a-z0-9\\-]*$")


slugify : String -> String
slugify string =
    string
        |> String.toLower
        |> replaceInvalid
        |> String.words
        |> String.join "-"
