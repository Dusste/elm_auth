module Signup exposing (Model, Msg, init, update, view)

import Api.Signup
import Components.Button
import Components.Element
import Components.Error
import Components.InputField
import Components.Misc
import Data.Credentials as Credentials
import Data.Ports as Ports
import Data.Validation as Validation
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Encode
import List.Extra


type alias Model =
    { storeEmail : String
    , storePassword : String
    , storeConfirmPassword : String
    , formState : FormState
    , errors : Dict String (List String)
    , showPasswords : List ( String, Bool )
    }


initialModel : Model
initialModel =
    { storeEmail = ""
    , storePassword = ""
    , storeConfirmPassword = ""
    , formState = Initial
    , errors = Dict.empty
    , showPasswords = []
    }


type FormState
    = Initial
    | Loading
    | Error String


type Msg
    = StoreEmail String
    | StorePassword String
    | StoreConfirmPassword String
    | SignupSubmit
    | SignupDone (Result Http.Error Credentials.Token)
    | ShowPassword String Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoreEmail email ->
            let
                resetErrorsPerField : Dict String (List String)
                resetErrorsPerField =
                    Validation.resetErrorsPerField "email" model.errors
            in
            ( { model
                | storeEmail = email
                , errors = resetErrorsPerField
              }
            , Cmd.none
            )

        StorePassword password ->
            let
                resetErrorsPerField : Dict String (List String)
                resetErrorsPerField =
                    Validation.resetErrorsPerField "password" model.errors
            in
            ( { model
                | storePassword = password
                , errors = resetErrorsPerField
              }
            , Cmd.none
            )

        StoreConfirmPassword confirmPassword ->
            let
                resetErrorsPerField : Dict String (List String)
                resetErrorsPerField =
                    Validation.resetErrorsPerField "confirm-password" model.errors
            in
            ( { model
                | storeConfirmPassword = confirmPassword
                , errors = resetErrorsPerField
              }
            , Cmd.none
            )

        SignupSubmit ->
            let
                validationConfig : Validation.Config
                validationConfig =
                    { validationRules =
                        [ { fieldName = "email"
                          , fieldRules =
                                [ Validation.CheckEmptyEmail
                                , Validation.CheckInvalidEmail
                                ]
                          , fieldValue = model.storeEmail
                          }
                        , { fieldName = "password"
                          , fieldRules =
                                [ Validation.CheckEmptyPassword
                                , Validation.CheckPasswordTooShort 10
                                , Validation.CheckPasswordCapitalize
                                , Validation.CheckPasswordSpecialChar
                                , Validation.CheckPasswordContainsInt
                                ]
                          , fieldValue = model.storePassword
                          }
                        , { fieldName = "confirm-password"
                          , fieldRules =
                                [ Validation.CheckPasswordMatch model.storePassword
                                ]
                          , fieldValue = model.storeConfirmPassword
                          }
                        ]
                    , initialErrors = model.errors
                    }

                potentialErrors : Dict String (List String)
                potentialErrors =
                    Validation.checkErrors validationConfig
            in
            if Validation.anyActiveError potentialErrors then
                ( { model
                    | errors = potentialErrors
                  }
                , Cmd.none
                )

            else
                ( { model
                    | errors = potentialErrors
                    , formState = Loading
                  }
                , Api.Signup.submitSignup { email = model.storeEmail, password = model.storePassword } SignupDone
                )

        SignupDone (Ok token) ->
            let
                tokenValue : Json.Encode.Value
                tokenValue =
                    Credentials.encodeToken token
            in
            ( { model | formState = Initial }
            , Ports.storeSession <| Just <| Json.Encode.encode 0 tokenValue
            )

        SignupDone (Err error) ->
            ( { model
                | formState =
                    Error <| Components.Error.buildErrorMessage error
              }
            , Cmd.none
            )

        ShowPassword id shouldShow ->
            ( { model | showPasswords = ( id, shouldShow ) :: model.showPasswords }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        shouldShowPassword : String -> Bool
        shouldShowPassword field =
            model.showPasswords
                |> List.Extra.find (\( id_, _ ) -> field == id_)
                |> Maybe.map Tuple.second
                >> Maybe.withDefault False
    in
    Components.Element.formLayout
        "Signup"
        [ Components.InputField.view
            |> Components.InputField.withValue model.storeEmail
            |> Components.InputField.withMsg StoreEmail
            |> Components.InputField.withType Components.InputField.Text
            |> Components.InputField.withDisable (model.formState == Loading)
            |> Components.InputField.withError (Components.Error.byFieldName "email" model.errors)
            |> Components.InputField.withExtraText (Components.InputField.Label "Email")
            |> Components.InputField.toHtml
        , Components.InputField.view
            |> Components.InputField.withValue model.storePassword
            |> Components.InputField.withMsg StorePassword
            |> Components.InputField.withType
                (Components.InputField.Password
                    ( "password"
                    , shouldShowPassword "password"
                    , ShowPassword
                    )
                )
            |> Components.InputField.withDisable (model.formState == Loading)
            |> Components.InputField.withError (Components.Error.byFieldName "password" model.errors)
            |> Components.InputField.withExtraText (Components.InputField.Label "Password")
            |> Components.InputField.toHtml
        , Components.InputField.view
            |> Components.InputField.withValue model.storeConfirmPassword
            |> Components.InputField.withMsg StoreConfirmPassword
            |> Components.InputField.withType
                (Components.InputField.Password
                    ( "confirm-password"
                    , shouldShowPassword "confirm-password"
                    , ShowPassword
                    )
                )
            |> Components.InputField.withDisable (model.formState == Loading)
            |> Components.InputField.withError (Components.Error.byFieldName "confirm-password" model.errors)
            |> Components.InputField.withExtraText (Components.InputField.Label "Confirm Password")
            |> Components.InputField.toHtml
        , Components.Button.view
            |> Components.Button.withText "Sign up"
            |> Components.Button.withMsg SignupSubmit
            |> Components.Button.withDisabled (model.formState == Loading)
            |> Components.Button.withPrimaryStyle
            |> Components.Button.toHtml
        , case model.formState of
            Initial ->
                Html.text ""

            Loading ->
                Components.Misc.loadingElement

            Error error ->
                Components.Element.notification (Components.Element.Error error)
        ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )
