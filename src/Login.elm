module Login exposing (Model, Msg, init, update, view)

import Api.Login
import Components.Button
import Components.Element
import Components.Error
import Components.InputField
import Components.Misc
import Data.Credentials as Credentials
import Data.OutMsg
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
    , formState : FormState
    , errors : Dict String (List String)
    , showPassword : ( String, Bool )
    }


initialModel : Model
initialModel =
    { storeEmail = ""
    , storePassword = ""
    , formState = Initial
    , errors = Dict.empty
    , showPassword = ( "password", False )
    }


type FormState
    = Initial
    | Loading
    | Error String


type Msg
    = StoreEmail String
    | StorePassword String
    | LoginSubmit
    | LoginDone (Result Http.Error Credentials.Token)
    | ShowPassword String Bool


update : Msg -> Model -> ( Model, List Data.OutMsg.OutMsg, Cmd Msg )
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
            , []
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
            , []
            , Cmd.none
            )

        LoginSubmit ->
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
                        ]
                    , initialErrors = model.errors
                    }

                potentialErrors : Dict String (List String)
                potentialErrors =
                    Validation.checkErrors validationConfig
            in
            if Validation.anyActiveError potentialErrors then
                ( { model | errors = potentialErrors }
                , []
                , Cmd.none
                )

            else
                ( { model
                    | errors = potentialErrors
                    , formState = Loading
                  }
                , []
                , Api.Login.submitLogin
                    { email = model.storeEmail
                    , password = model.storePassword
                    }
                    LoginDone
                )

        LoginDone (Ok token) ->
            let
                tokenValue =
                    Credentials.encodeToken token
            in
            ( { model | formState = Initial }
            , [ Data.OutMsg.RedirectToProfile token ]
            , Ports.storeSession <| Just <| Json.Encode.encode 0 tokenValue
            )

        LoginDone (Err error) ->
            ( { model | formState = Error <| Components.Error.buildErrorMessage error }
            , []
            , Cmd.none
            )

        ShowPassword id shouldShow ->
            ( { model | showPassword = ( id, shouldShow ) }
            , []
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Components.Element.formLayout
        "Login"
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
                    , model.showPassword |> Tuple.second
                    , ShowPassword
                    )
                )
            |> Components.InputField.withDisable (model.formState == Loading)
            |> Components.InputField.withError (Components.Error.byFieldName "password" model.errors)
            |> Components.InputField.withExtraText (Components.InputField.Label "Password")
            |> Components.InputField.toHtml
        , Html.div
            [ HA.class "flex justify-between items-center" ]
            [ Components.Button.view
                |> Components.Button.withText "Sign in"
                |> Components.Button.withMsg LoginSubmit
                |> Components.Button.withDisabled (model.formState == Loading)
                |> Components.Button.withPrimaryStyle
                |> Components.Button.toHtml
            , Components.Button.view
                |> Components.Button.withText "Forgot password ?"
                |> Components.Button.withUrl "/forgot-password"
                |> Components.Button.withDisabled (model.formState == Loading)
                |> Components.Button.withLinkStyle
                |> Components.Button.toHtml
            ]
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
