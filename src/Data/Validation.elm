module Data.Validation exposing
    ( Config
    , Validation(..)
    , anyActiveError
    , checkErrors
    , resetErrorsPerField
    , updateError
    )

import Data.Util
import Dict exposing (Dict)


type alias Config =
    { validationRules : List ValidationRule
    , initialErrors : Dict String (List String)
    }


type alias ValidationRule =
    { fieldName : String
    , fieldRules : List (String -> Validation)
    , fieldValue : String
    }


type Validation
    = CheckEmptyEmail String
    | CheckInvalidEmail String
    | CheckEmptyName String
    | CheckEmptyPassword String
    | CheckPasswordTooShort Int String
    | CheckPasswordCapitalize String
    | CheckPasswordSpecialChar String
    | CheckPasswordContainsInt String
    | CheckPasswordMatch String String


validationToErrorMsg : Validation -> String
validationToErrorMsg validation =
    case validation of
        -- TODO more intuitive way to know what to print
        CheckEmptyEmail _ ->
            "Email is empty"

        CheckEmptyName _ ->
            "Name is empty"

        CheckInvalidEmail _ ->
            "Email is invalid"

        CheckEmptyPassword _ ->
            "Password is empty"

        CheckPasswordTooShort _ _ ->
            "Password must have at least 10 characters"

        CheckPasswordCapitalize _ ->
            "Password must contain at least one capitalized letter"

        CheckPasswordSpecialChar _ ->
            "Password must contain at least one special character"

        CheckPasswordContainsInt _ ->
            "Password must contain at least one number"

        CheckPasswordMatch _ _ ->
            "Passwors doesn't match"


checkErrors : Config -> Dict String (List String)
checkErrors { validationRules, initialErrors } =
    validationRules
        |> List.foldr
            (\{ fieldName, fieldRules, fieldValue } sumErrors ->
                fieldRules
                    |> List.map
                        (\toValidation ->
                            ( fieldName, toValidation, fieldValue ) :: sumErrors
                        )
                    |> List.concat
            )
            []
        |> List.foldr
            (\( fieldName, toValidation, fieldValue ) sumErrors ->
                updateError fieldName sumErrors (toValidation fieldValue)
            )
            initialErrors


updateError :
    String
    -> Dict String (List String)
    -> Validation
    -> Dict String (List String)
updateError field errors validation =
    let
        errorMsg : String
        errorMsg =
            validationToErrorMsg validation
    in
    if Dict.member field errors then
        Dict.update
            field
            (Maybe.map
                (modifyErrorPerField (shouldInsertError validation) errorMsg)
            )
            errors

    else if shouldInsertError validation then
        Dict.insert field [ errorMsg ] errors

    else
        errors


shouldInsertError : Validation -> Bool
shouldInsertError validation =
    case validation of
        CheckEmptyEmail email ->
            Data.Util.checkEmpty email

        CheckInvalidEmail email ->
            not <| Data.Util.isValidEmail email

        CheckEmptyName name ->
            Data.Util.checkEmpty name

        CheckEmptyPassword password ->
            Data.Util.checkEmpty password

        CheckPasswordTooShort minimum password ->
            Data.Util.checkLength password minimum

        CheckPasswordCapitalize password ->
            not <| Data.Util.checkCapitalized password

        CheckPasswordSpecialChar password ->
            not <| Data.Util.checkSpecChar password

        CheckPasswordContainsInt password ->
            not <| Data.Util.checkInt password

        CheckPasswordMatch password confirmPassword ->
            not <| Data.Util.checkMatch password confirmPassword


modifyErrorPerField : Bool -> String -> List String -> List String
modifyErrorPerField checkError errorMsg errs =
    case ( checkError, List.member errorMsg errs ) of
        ( False, True ) ->
            List.filter (\err -> err /= errorMsg) errs

        ( True, False ) ->
            List.append errs [ errorMsg ]

        _ ->
            errs


anyActiveError : Dict String (List String) -> Bool
anyActiveError errors =
    errors
        |> Dict.values
        |> List.all List.isEmpty
        |> not


resetErrorsPerField : String -> Dict String (List String) -> Dict String (List String)
resetErrorsPerField key errors =
    Dict.filter (\k _ -> k /= key) errors
