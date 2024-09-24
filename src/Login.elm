module Login exposing (Model, Msg, init, update, view)

import Api.Login
import Components.Element
import Components.Error
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
    }


initialModel : Model
initialModel =
    { storeEmail = ""
    , storePassword = ""
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
    | LoginSubmit
    | LoginDone (Result Http.Error Credentials.Token)


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
            ( { model | errors = potentialErrors }
            , []
            , if Validation.anyActiveError potentialErrors then
                Cmd.none

              else
                Api.Login.submitLogin
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


view : Model -> Html Msg
view model =
    Html.div
        [ HA.class "flex justify-center mt-32" ]
        [ Html.div
            [ HA.class "flex flex-col w-[300px] gap-y-4"
            ]
            [ Html.h2
                []
                [ Html.text "Login" ]
            , case model.formState of
                Loading ->
                    Components.Misc.loadingElement

                Error error ->
                    Html.p
                        [ HA.class "text-red-500"
                        ]
                        [ Html.text error ]

                Initial ->
                    Html.text ""
            , Html.form
                [ --  HA.class [ Tw.flex, Tw.flex_col, Tw.gap_5, Tw.text_xl, Tw.w_full, Bp.md [ Tw.w_60 ] ]
                  HA.class "flex flex-col gap-y-4"
                ]
                [ Html.div
                    [-- HA.class [ Tw.flex, Tw.flex_col, Tw.gap_3 ]
                    ]
                    [ Components.Element.inputField
                        { type_ = Components.Element.Text
                        , label = Just "Email"
                        , value = model.storeEmail
                        , toMsg = StoreEmail
                        , isDisabled = False
                        , error = Components.Error.byFieldName "email" model.errors
                        }
                    ]
                , Html.div
                    [-- HA.class [ Tw.flex, Tw.flex_col, Tw.gap_3 ]
                    ]
                    [ Components.Element.inputField
                        { type_ = Components.Element.Password
                        , label = Just "Password"
                        , value = model.storePassword
                        , toMsg = StorePassword
                        , isDisabled = False
                        , error = Components.Error.byFieldName "password" model.errors
                        }
                    ]
                , Html.div
                    [ HA.class "mt-4 flex items-center justify-between" ]
                    [ Components.Element.button
                        |> Components.Element.withText "Sign in"
                        |> Components.Element.withMsg LoginSubmit
                        |> Components.Element.withDisabled False
                        |> Components.Element.withPrimaryStyle
                        |> Components.Element.toHtml
                    , Html.a
                        [ HA.href "/forgot-password"

                        -- , HA.class [ Tw.mt_5 ]
                        ]
                        [ Html.text "Forgot password ?" ]
                    ]
                ]
            ]
        ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )
