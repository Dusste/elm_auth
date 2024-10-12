module ForgotPassword exposing (..)

import Api.ForgotPassword
import Components.Button
import Components.Element
import Components.Error
import Components.InputField
import Components.Misc
import Data.Validation as Validation
import Dict exposing (Dict)
import Html exposing (Html, text)
import Html.Attributes as HA
import Html.Events as HE
import Http


type alias Model =
    { storeEmail : String
    , formState : FormState
    , errors : Dict String (List String)
    }


type Msg
    = StoreEmail String
    | Submit
    | Done (Result Http.Error ())


type FormState
    = Initial
    | Loading
    | Success
    | Error String


initialModel : Model
initialModel =
    { storeEmail = ""
    , formState = Initial
    , errors = Dict.empty
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    Components.Element.formLayout
        "Password reset request"
        [ Components.Element.notification
            (Components.Element.Info "Fill in email and we'll make sure you can reset password")
        , Components.InputField.view
            |> Components.InputField.withValue model.storeEmail
            |> Components.InputField.withMsg StoreEmail
            |> Components.InputField.withType Components.InputField.Text
            |> Components.InputField.withDisable (model.formState == Loading)
            |> Components.InputField.withError (Components.Error.byFieldName "email" model.errors)
            |> Components.InputField.withExtraText (Components.InputField.Label "Email")
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
                Html.div
                    []
                    [ Components.Element.notification
                        (Components.Element.Success "Submitted successfully !")
                    , Html.p
                        []
                        [ text "Check your email for rest link." ]
                    ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit ->
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
                        ]
                    , initialErrors = model.errors
                    }

                potentialErrors : Dict String (List String)
                potentialErrors =
                    Validation.checkErrors validationConfig
            in
            if Validation.anyActiveError potentialErrors then
                ( { model | errors = potentialErrors }, Cmd.none )

            else
                ( { model | errors = potentialErrors, formState = Loading }
                , Api.ForgotPassword.submitForgotPassword model.storeEmail Done
                )

        StoreEmail str ->
            let
                resetErrorsPerField : Dict String (List String)
                resetErrorsPerField =
                    Validation.resetErrorsPerField "email" model.errors
            in
            ( { model
                | storeEmail = str
                , errors = resetErrorsPerField
              }
            , Cmd.none
            )

        Done (Ok _) ->
            ( { model | formState = Success, storeEmail = "" }, Cmd.none )

        Done (Err err) ->
            ( { model | formState = Error <| Components.Error.buildErrorMessage err }, Cmd.none )
