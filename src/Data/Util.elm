module Data.Util exposing
    ( checkCapitalized
    , checkEmpty
    , checkLength
    , checkMatch
    , checkSpecChar
    , loadingElement
    , validEmail
    )

import Html exposing (Html)
import Regex exposing (Regex)



-- todo: try to find something better


validEmail : Regex
validEmail =
    "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
        |> Regex.fromStringWith { caseInsensitive = True, multiline = False }
        |> Maybe.withDefault Regex.never


loadingElement : Html msg
loadingElement =
    Html.div
        [--  HA.class [ Tw.relative, Tw.h_5, Tw.w_5, Tw.flex ]
        ]
        [ Html.span
            [-- HA.class [ Tw.animate_ping, Tw.absolute, Tw.inline_flex, Tw.h_full, Tw.w_full, Tw.rounded_full, Tw.bg_color Tw.sky_400, Tw.opacity_75 ]
            ]
            []
        , Html.span
            [-- HA.class [ Tw.relative, Tw.inline_flex, Tw.rounded_full, Tw.h_5, Tw.w_5, Tw.bg_color Tw.sky_500 ]
            ]
            []
        ]


specialCharacterRegex : Regex.Regex
specialCharacterRegex =
    Regex.fromString "[!-\\/:-@[-`{-~]"
        |> Maybe.withDefault Regex.never


checkEmpty : String -> Bool
checkEmpty str =
    let
        trim : String
        trim =
            String.trim str
    in
    String.length trim == 0


checkLength : String -> Int -> Bool
checkLength str min =
    let
        trim : String
        trim =
            String.trim str
    in
    String.length trim < min


checkCapitalized : String -> Bool
checkCapitalized str =
    let
        trim : String
        trim =
            String.trim str
    in
    String.any Char.isUpper trim


checkSpecChar : String -> Bool
checkSpecChar str =
    let
        trim : String
        trim =
            String.trim str
    in
    Regex.contains specialCharacterRegex trim


checkMatch : String -> String -> Bool
checkMatch str1 str2 =
    let
        trim1 : String
        trim1 =
            String.trim str1

        trim2 : String
        trim2 =
            String.trim str2
    in
    trim1 == trim2
