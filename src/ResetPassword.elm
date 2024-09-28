module ResetPassword exposing (..)

import Api.ResetPassword
import Components.Element
import Components.Error
import Components.Misc
import Data.Validation as Validation
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http


type alias Model =
    { storePassword : String
    , storeConfirmPassword : String
    , formState : FormState
    , resetCodeParam : String
    , errors : Dict String (List String)
    }


type Msg
    = StorePassword String
    | StoreConfirmPassword String
    | Submit
    | Done (Result Http.Error ())


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
    }


init : String -> ( Model, Cmd Msg )
init resetCodeParam =
    ( { initialModel | resetCodeParam = resetCodeParam }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    Html.div
        [ HA.class "flex justify-center mt-32" ]
        [ Html.div
            [ HA.class "flex flex-col w-[300px] gap-y-4" ]
            [ Html.h2
                []
                [ Html.text "Reset password" ]
            , Html.form
                [ HA.class "flex flex-col gap-y-4" ]
                [ Components.Element.inputField
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
                    |> Components.Element.withText "Submit"
                    |> Components.Element.withMsg Submit
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

                    Success ->
                        Html.div
                            []
                            [ Components.Element.notification
                                (Components.Element.Success "Your password has been reset. Please login with your new password.")

                            -- TODO send out msg to login
                            -- , Components.Element.button
                            --     |> Components.Element.withText "Login"
                            --     |> Components.Element.withMsg Login
                            --     |> Components.Element.withDisabled False
                            --     |> Components.Element.withPrimaryStyle
                            --     |> Components.Element.toHtml
                            ]
                ]
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
            ( { model | formState = Success, storePassword = "", storeConfirmPassword = "" }, Cmd.none )

        Done (Err err) ->
            ( { model | formState = Error <| Components.Error.buildErrorMessage err }, Cmd.none )
