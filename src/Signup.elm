module Signup exposing (Model, Msg, init, update, view)

import Api.Signup
import Components.Element
import Components.Error
import Data.Credentials as Credentials
import Data.Ports as Ports
import Data.Util as Util
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
            ( { model | storeEmail = email }, Cmd.none )

        StorePassword password ->
            ( { model | storePassword = password }, Cmd.none )

        StoreConfirmPassword confirmPassword ->
            ( { model | storeConfirmPassword = confirmPassword }, Cmd.none )

        SignupSubmit ->
            let
                errors : Dict String (List String)
                errors =
                    model.errors
                        |> Components.Error.updateError
                            (Components.Error.CheckEmptyEmail model.storeEmail)
                            "email"
                        |> Components.Error.updateError
                            (Components.Error.CheckInvalidEmail model.storeEmail)
                            "email"
                        |> Components.Error.updateError
                            (Components.Error.CheckEmptyPassword model.storePassword)
                            "password"
                        |> Components.Error.updateError
                            (Components.Error.CheckPasswordTooShort model.storePassword 10)
                            "password"
                        |> Components.Error.updateError
                            (Components.Error.CheckPasswordCapitalize model.storePassword)
                            "password"
                        |> Components.Error.updateError
                            (Components.Error.CheckPasswordSpecialChar model.storePassword)
                            "password"
                        |> Components.Error.updateError
                            (Components.Error.CheckEmptyPassword model.storeConfirmPassword)
                            "confirm-password"
                        |> Components.Error.updateError
                            (Components.Error.CheckPasswordTooShort model.storeConfirmPassword 10)
                            "confirm-password"
                        |> Components.Error.updateError
                            (Components.Error.CheckPasswordCapitalize model.storeConfirmPassword)
                            "confirm-password"
                        |> Components.Error.updateError
                            (Components.Error.CheckPasswordSpecialChar model.storeConfirmPassword)
                            "confirm-password"
                        |> Components.Error.updateError
                            (Components.Error.CheckPasswordMatch model.storePassword model.storeConfirmPassword)
                            "confirm-password"
            in
            ( { model | errors = errors }
            , if Components.Error.anyActiveError errors then
                Cmd.none

              else
                Api.Signup.submitSignup { email = model.storeEmail, password = model.storePassword } SignupDone
            )

        SignupDone (Ok token) ->
            let
                tokenValue =
                    Credentials.encodeToken token
            in
            ( { model | formState = Initial }, Ports.storeSession <| Just <| Json.Encode.encode 0 tokenValue )

        SignupDone (Err error) ->
            ( { model | formState = Error <| Components.Error.buildErrorMessage error }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div
        [--  HA.class [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_6, Tw.relative, Bp.md [ Tw.m_20 ] ]
        ]
        [ Html.h2
            [--  HA.class [ Tw.text_3xl ]
            ]
            [ Html.text "Signup" ]
        , case model.formState of
            Loading ->
                Html.div
                    [-- HA.class [ Tw.absolute, Tw.w_full, Tw.h_full, Tw.flex, Tw.justify_center, Tw.items_center, Tw.bg_color Tw.sky_50, Tw.bg_opacity_40 ]
                    ]
                    [ Util.loadingElement ]

            Error error ->
                Html.p
                    [--  HA.class [ Tw.text_color Tw.red_400 ]
                    ]
                    [ Html.text error ]

            Initial ->
                Html.text ""
        , Html.form
            [-- HA.class [ Tw.flex, Tw.flex_col, Tw.gap_5, Tw.text_xl, Tw.w_full, Bp.md [ Tw.w_60 ] ]
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
                [--  HA.class [ Tw.flex, Tw.flex_col, Tw.gap_3 ]
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
                [--  HA.class [ Tw.flex, Tw.flex_col, Tw.gap_3 ]
                ]
                [ Components.Element.inputField
                    { type_ = Components.Element.Password
                    , label = Just "Confirm Password"
                    , value = model.storeConfirmPassword
                    , toMsg = StoreConfirmPassword
                    , isDisabled = False
                    , error = Components.Error.byFieldName "confirm-password" model.errors
                    }
                ]
            , Html.button
                [ -- /HA.class Gs.buttonStyle
                  HA.type_ "button"
                , HE.onClick SignupSubmit
                ]
                [ Html.text "Sign up" ]
            ]
        ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )
