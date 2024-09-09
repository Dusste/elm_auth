module Components.Error exposing
    ( Validation(..)
    , anyActiveError
    , buildErrorMessage
    , byFieldName
    , updateError
    )

import Components.Element
import Data.User
import Data.Util
import Dict exposing (Dict)
import Html exposing (Html)
import Http
import Json.Decode
import Json.Decode.Pipeline


type alias ErrorField =
    Dict String (List String)


type alias ErrorResponse =
    { type_ : String
    , reference : String
    , description : String
    }


type Validation
    = CheckEmptyEmail String
    | CheckInvalidEmail String
    | CheckEmptyPassword String
    | CheckPasswordTooShort String Int
    | CheckPasswordCapitalize String
    | CheckPasswordSpecialChar String
    | CheckPasswordMatch String String


validationToErrorMsg : Validation -> String
validationToErrorMsg validation =
    case validation of
        CheckEmptyEmail _ ->
            "Email is empty"

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

        CheckPasswordMatch _ _ ->
            "Passwors doesn't match"


type CustomError
    = Concrete ErrorResponse
    | Naive Http.Error



{-
   Why `hasError` ?
   - Easy to pipeline/chain in case you need to update
   more then one error field
   ex:
    constructErrors =
        model.errors
            |> ErrorData.updateError isEmpty "Field A" "Some error message"
            |> ErrorData.updateError isInvalid "Field B" "Other error message"
-}


updateError : Validation -> String -> Dict String (List String) -> Dict String (List String)
updateError validation field errors =
    let
        errorMsg : String
        errorMsg =
            validationToErrorMsg validation

        shouldInsertError : Bool
        shouldInsertError =
            case validation of
                CheckEmptyEmail email ->
                    Data.Util.checkEmpty email

                CheckInvalidEmail email ->
                    not <| Data.User.isEmailValid email

                CheckEmptyPassword password ->
                    Data.Util.checkEmpty password

                CheckPasswordTooShort password minimum ->
                    Data.Util.checkLength password minimum

                CheckPasswordCapitalize password ->
                    not <| Data.Util.checkCapitalized password

                CheckPasswordSpecialChar password ->
                    not <| Data.Util.checkSpecChar password

                CheckPasswordMatch password confirmPassword ->
                    not <| Data.Util.checkMatch password confirmPassword
    in
    if Dict.member field errors then
        Dict.update
            field
            (Maybe.map
                (\errors_ ->
                    updateErrorFlow shouldInsertError errorMsg errors_
                )
            )
            errors

    else if shouldInsertError then
        Dict.insert field [ errorMsg ] errors

    else
        errors


updateErrorFlow : Bool -> String -> List String -> List String
updateErrorFlow checkError errorMsg errs =
    case ( checkError, List.member errorMsg errs ) of
        ( False, True ) ->
            List.filter (\err -> err /= errorMsg) errs

        ( True, False ) ->
            List.append errs [ errorMsg ]

        _ ->
            errs


customErrorDecoder : Json.Decode.Decoder CustomError
customErrorDecoder =
    Json.Decode.succeed ErrorResponse
        |> Json.Decode.Pipeline.required "type" Json.Decode.string
        |> Json.Decode.Pipeline.required "reference" Json.Decode.string
        |> Json.Decode.Pipeline.required "description" Json.Decode.string
        |> Json.Decode.map (\errResponse -> Concrete errResponse)


httpBadStatus : String -> Result CustomError value
httpBadStatus jsonStringifyResponse =
    let
        customError =
            Json.Decode.decodeString customErrorDecoder jsonStringifyResponse
    in
    case customError of
        Ok (Concrete customErrorRec) ->
            Err <| Concrete customErrorRec

        Ok (Naive _) ->
            Err <| Naive (Http.BadBody "Something went wrong")

        Err decodeError ->
            Err <| Naive (Http.BadBody (Json.Decode.errorToString decodeError))


buildCustomErrorMsg : a -> Http.Response String -> Result CustomError a
buildCustomErrorMsg customResponse response =
    case response of
        Http.BadUrl_ url ->
            Err <| Naive (Http.BadUrl url)

        Http.Timeout_ ->
            Err <| Naive Http.Timeout

        Http.BadStatus_ metadata jsonStringifyResponse ->
            httpBadStatus jsonStringifyResponse

        Http.NetworkError_ ->
            Err <| Naive Http.NetworkError

        Http.GoodStatus_ _ _ ->
            Ok customResponse


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message


byFieldName : String -> ErrorField -> List String
byFieldName field errors =
    errors
        |> Dict.get field
        |> Maybe.withDefault []


anyActiveError : ErrorField -> Bool
anyActiveError errors =
    errors
        |> Dict.values
        |> List.all List.isEmpty
        |> not


toHtml : String -> ErrorField -> Html msg
toHtml field errors =
    let
        errors_ =
            errors
                |> Dict.get field
                |> Maybe.withDefault []
    in
    Html.div []
        (List.map
            (\error ->
                Components.Element.notification (Components.Element.Error error)
            )
            errors_
        )


resetAllErrors : ErrorField -> ErrorField
resetAllErrors errors =
    Dict.map (\_ _ -> []) errors
