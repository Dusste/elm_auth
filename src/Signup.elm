module Signup exposing (Model, Msg, init, update, view)

import Api.Signup
import Components.Element
import Components.Error
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


type alias Model =
    { storeEmail : String
    , storePassword : String
    , storeConfirmPassword : String
    , formState : FormState
    , errors : Dict String (List String)
    }


initialModel : Model
initialModel =
    { storeEmail = ""
    , storePassword = ""
    , storeConfirmPassword = ""
    , formState = Initial
    , errors = Dict.empty
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
                                [ Validation.CheckEmptyPassword
                                , Validation.CheckPasswordTooShort 10
                                , Validation.CheckPasswordCapitalize
                                , Validation.CheckPasswordSpecialChar
                                , Validation.CheckPasswordContainsInt
                                , Validation.CheckPasswordMatch model.storePassword
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


view : Model -> Html Msg
view model =
    Components.Element.formLayout
        "Signup"
        [ Components.Element.inputField
            { type_ = Components.Element.Text
            , label = Just "Email"
            , value = model.storeEmail
            , toMsg = StoreEmail
            , isDisabled = model.formState == Loading
            , error = Components.Error.byFieldName "email" model.errors
            }
        , Components.Element.inputField
            { type_ = Components.Element.Password
            , label = Just "Password"
            , value = model.storePassword
            , toMsg = StorePassword
            , isDisabled = model.formState == Loading
            , error = Components.Error.byFieldName "password" model.errors
            }
        , Components.Element.inputField
            { type_ = Components.Element.Password
            , label = Just "Confirm Password"
            , value = model.storeConfirmPassword
            , toMsg = StoreConfirmPassword
            , isDisabled = model.formState == Loading
            , error = Components.Error.byFieldName "confirm-password" model.errors
            }
        , Components.Element.button
            |> Components.Element.withText "Sign up"
            |> Components.Element.withMsg SignupSubmit
            |> Components.Element.withDisabled (model.formState == Loading)
            |> Components.Element.withPrimaryStyle
            |> Components.Element.toHtml
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
