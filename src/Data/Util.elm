module Data.Util exposing
    ( checkCapitalized
    , checkEmpty
    , checkInt
    , checkLength
    , checkMatch
    , checkSpecChar
    , isValidEmail
    )

import Html exposing (Html)
import Regex
import String.Extra


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


checkInt : String -> Bool
checkInt str =
    let
        trim : String
        trim =
            String.trim str
    in
    Regex.contains
        (Regex.fromString "\\d"
            |> Maybe.withDefault Regex.never
        )
        trim


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


{-| (Still) Uncomplete email check
-}
isValidEmail : String -> Bool
isValidEmail input =
    case String.split "@" input of
        [ beforeEt, afterEtWithDot ] ->
            let
                beforeEtIsValid : Bool
                beforeEtIsValid =
                    (String.length beforeEt > 0) && not (String.contains " " beforeEt)

                afterEtWithDotIsValid : Bool
                afterEtWithDotIsValid =
                    case String.split "." afterEtWithDot of
                        [ afterEt, afterDot ] ->
                            let
                                validate v =
                                    not (String.Extra.isBlank v) && not (String.contains " " v)
                            in
                            validate afterEt && validate afterDot

                        _ ->
                            False
            in
            beforeEtIsValid && afterEtWithDotIsValid

        _ ->
            False
