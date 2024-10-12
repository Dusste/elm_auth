module ResetPassword exposing (..)

import Api.ResetPassword
import Components.Button
import Components.Element
import Components.Error
import Components.InputField
import Components.Misc
import Data.Validation as Validation
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import List.Extra


type alias Model =
    { storePassword : String
    , storeConfirmPassword : String
    , formState : FormState
    , resetCodeParam : String
    , errors : Dict String (List String)
    , showPasswords : List ( String, Bool )
    }


type Msg
    = StorePassword String
    | StoreConfirmPassword String
    | Submit
    | Done (Result Http.Error ())
    | ShowPassword String Bool


type FormState
    = Initial
    | Loading
    | Success
    | Error String


initialModel : Model
initialModel =
    { storePassword = ""
    , storeConfirmPassword = ""
    , formState = Initial
    , resetCodeParam = ""
    , errors = Dict.empty
    , showPasswords = []
    }


init : String -> ( Model, Cmd Msg )
init resetCodeParam =
    ( { initialModel | resetCodeParam = resetCodeParam }
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
        "Reset password"
        [ Components.InputField.view
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
            |> Components.Button.withText "Submit"
            |> Components.Button.withMsg Submit
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

            Success ->
                Components.Element.notification
                    (Components.Element.Success "Your password has been reset")

        -- TODO send out msg to login
        -- , Components.Element.button
        --     |> Components.Element.withText "Login"
        --     |> Components.Element.withMsg Login
        --     |> Components.Element.withDisabled False
        --     |> Components.Element.withPrimaryStyle
        --     |> Components.Element.toHtml
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit ->
            let
                validationConfig : Validation.Config
                validationConfig =
                    { validationRules =
                        [ { fieldName = "password"
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
                                [ Validation.CheckPasswordMatch model.storePassword ]
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
                ( { model | errors = potentialErrors }
                , Cmd.none
                )

            else
                ( { model | errors = potentialErrors, formState = Loading }
                , Api.ResetPassword.submitResetPassword model.storeConfirmPassword model.resetCodeParam Done
                )

        StorePassword str ->
            let
                resetErrorsPerField : Dict String (List String)
                resetErrorsPerField =
                    Validation.resetErrorsPerField "password" model.errors
            in
            ( { model
                | storePassword = str
                , errors = resetErrorsPerField
              }
            , Cmd.none
            )

        StoreConfirmPassword str ->
            let
                resetErrorsPerField : Dict String (List String)
                resetErrorsPerField =
                    Validation.resetErrorsPerField "confirm-password" model.errors
            in
            ( { model
                | storeConfirmPassword = str
                , errors = resetErrorsPerField
              }
            , Cmd.none
            )

        Done (Ok _) ->
            ( { model
                | formState = Success
                , storePassword = ""
                , storeConfirmPassword = ""
              }
            , Cmd.none
            )

        Done (Err err) ->
            ( { model | formState = Error <| Components.Error.buildErrorMessage err }
            , Cmd.none
            )

        ShowPassword id shouldShow ->
            ( { model | showPasswords = ( id, shouldShow ) :: model.showPasswords }
            , Cmd.none
            )
