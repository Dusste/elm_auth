module Data.User exposing
    ( Email
    , Password
    , ValidCredentials
    , andThenValidateConfirmPassword
    , credentialsEncoder
    , emailEncoder
    , fromEmailToString
    , fromStringToValidEmail
    , isEmailValid
    , parseEmail
    , passwordEncoder
    , validateConfirmPassword
    , validateCredentials
    )

import Json.Encode as Encode
import Parser as P exposing ((|.), (|=), Parser, deadEndsToString)


type Email
    = Email String


type Password
    = Password String


type alias ValidCredentials =
    { email : Email
    , password : Password
    }


credentialsEncoder : ValidCredentials -> Encode.Value
credentialsEncoder { email, password } =
    Encode.object
        [ ( "email", emailEncoder email )
        , ( "password", passwordEncoder password )
        ]


emailEncoder : Email -> Encode.Value
emailEncoder email =
    Encode.string <| fromEmailToString email


passwordEncoder : Password -> Encode.Value
passwordEncoder password =
    Encode.string <| fromPasswordToString password


fromEmailToString : Email -> String
fromEmailToString (Email validEmail) =
    validEmail


fromPasswordToString : Password -> String
fromPasswordToString (Password validPassword) =
    validPassword


fromStringToValidEmail : String -> Result String Email
fromStringToValidEmail email =
    let
        trimmedEmail =
            String.trim email
    in
    if String.isEmpty trimmedEmail then
        Err "Email can't be empty"

    else
        parseEmail trimmedEmail


isEmailValid : String -> Bool
isEmailValid email =
    case fromStringToValidEmail email of
        Ok _ ->
            True

        Err _ ->
            False



-- BeforeEt "dooshanstevanovic" | AfterEr "gmail" | AfterDot "com"


type SplitEmail
    = BeforeEt String
    | AfterEt String
    | AfterDot String


type alias ConstructEmail =
    { beforeEt : String, afterEt : String, afterDot : String }


beforeEtParser : Parser SplitEmail
beforeEtParser =
    P.succeed BeforeEt
        |= (P.chompWhile Char.isAlphaNum
                |> P.getChompedString
                |> P.andThen
                    (\beforeEt ->
                        if String.isEmpty beforeEt then
                            --P.problem "Email does not contain username"
                            P.problem "Invalid email"

                        else
                            P.succeed beforeEt
                    )
           )


afterEtParser : Parser SplitEmail
afterEtParser =
    P.succeed AfterEt
        |= (P.chompWhile Char.isAlphaNum
                |> P.getChompedString
                |> P.andThen
                    (\afterEt ->
                        let
                            _ =
                                Debug.log "afterEt" afterEt
                        in
                        if String.isEmpty afterEt then
                            --P.problem "Email does not contain mail server"
                            P.problem "Invalid email"

                        else
                            P.succeed afterEt
                    )
           )


afterDotParser : Parser SplitEmail
afterDotParser =
    P.succeed AfterDot
        |= (P.chompWhile Char.isAlphaNum
                |> P.getChompedString
                |> P.andThen
                    (\afterDot ->
                        if String.isEmpty afterDot then
                            -- P.problem "Email does not contain domain"
                            P.problem "Invalid email"

                        else
                            P.succeed afterDot
                    )
           )


emailParser : Parser (Maybe ConstructEmail)
emailParser =
    P.succeed
        (\a b c ->
            case ( a, b, c ) of
                ( BeforeEt be, AfterEt ae, AfterDot ad ) ->
                    Just <| ConstructEmail be ae ad

                _ ->
                    Nothing
        )
        |= beforeEtParser
        |. P.symbol "@"
        |= afterEtParser
        |. P.symbol "."
        |= afterDotParser


parseEmail : String -> Result String Email
parseEmail email =
    case P.run emailParser email of
        Err _ ->
            Err "Invalid email"

        Ok _ ->
            Ok (Email email)


fromStringToValidPassword : String -> Result String Password
fromStringToValidPassword password =
    let
        trimmedPassword =
            String.trim password
    in
    if String.isEmpty trimmedPassword then
        Err "Password can't be empty"

    else if String.length trimmedPassword < 10 then
        Err "Password can't be less then 10 characters"

    else
        Ok (Password trimmedPassword)


andThenValidateConfirmPassword : String -> Result String { a | password : Password } -> Result String { a | password : Password }
andThenValidateConfirmPassword confirmPassword resultCredential =
    resultCredential
        |> Result.andThen
            (\cred ->
                let
                    validPassword =
                        fromPasswordToString cred.password

                    trimmedConfirmPassword =
                        String.trim confirmPassword
                in
                if String.length trimmedConfirmPassword < 10 then
                    Err "Confirm password can't be less then 10 characters"

                else if validPassword /= trimmedConfirmPassword then
                    Err "Passwords doesn't match"

                else if String.isEmpty trimmedConfirmPassword then
                    Err "Confirm password can't be empty"

                else
                    Ok { cred | password = cred.password }
            )


validateConfirmPassword : { password : String, confirmPassword : String } -> Result String { password : Password }
validateConfirmPassword { password, confirmPassword } =
    fromStringToValidPassword password
        |> Result.map
            (\okPassword ->
                { password = okPassword }
            )
        |> andThenValidateConfirmPassword confirmPassword


validateCredentials : { email : String, password : String } -> Result String ValidCredentials
validateCredentials { email, password } =
    Result.map2
        ValidCredentials
        (fromStringToValidEmail email)
        (fromStringToValidPassword password)
