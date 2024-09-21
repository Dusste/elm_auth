module ForgotPassword exposing (..)

import Api.ForgotPassword
import Components.Element
import Components.Error
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
    Html.div
        [ HA.class "flex justify-center mt-32"
        ]
        [ Html.div
            [ HA.class "flex flex-col w-[300px] gap-y-4" ]
            [ Html.h2
                []
                [ text "Forgot Password" ]
            , case model.formState of
                Loading ->
                    Components.Misc.loadingElement

                Error error ->
                    Html.p
                        [ HA.class "text-red-500"
                        ]
                        [ text error ]

                Initial ->
                    Html.p
                        []
                        [ text "Enter your email and we will send you a reset link." ]

                Success ->
                    Html.div
                        []
                        [ Components.Element.notification
                            (Components.Element.Success "Submitted successfully !")
                        , Html.p
                            [ HA.class "mt-4" ]
                            [ text "Check your email for rest link." ]
                        ]
            , Html.form
                [ HA.class "flex flex-col gap-y-4"
                ]
                [ Components.Element.inputField
                    { type_ = Components.Element.Text
                    , label = Just "Email"
                    , value = model.storeEmail
                    , toMsg = StoreEmail
                    , isDisabled = False
                    , error = Components.Error.byFieldName "email" model.errors
                    }
                , Html.div
                    [ HA.class "mt-4" ]
                    [ Components.Element.button
                        |> Components.Element.withText "Submit"
                        |> Components.Element.withMsg Submit
                        |> Components.Element.withDisabled False
                        |> Components.Element.withPrimaryStyle
                        |> Components.Element.toHtml
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
            ( { model | errors = potentialErrors }
            , if Validation.anyActiveError potentialErrors then
                Cmd.none

              else
                Api.ForgotPassword.submitForgotPassword model.storeEmail Done
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
