module Signup exposing (Model, Msg, init, update, view)

import Api.Signup
import Components.Error
import Data.Credentials as Credentials
import Data.Ports as Ports
import Data.User as User
import Data.Util as Util
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
    }


initialModel : Model
initialModel =
    { storeEmail = ""
    , storePassword = ""
    , storeConfirmPassword = ""
    , formState = Initial
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
                validatedCred : Result String User.ValidCredentials
                validatedCred =
                    User.validateCredentials { email = model.storeEmail, password = model.storePassword }
                        |> User.andThenValidateConfirmPassword model.storeConfirmPassword
            in
            case validatedCred of
                Err error ->
                    ( { model | formState = Error error }, Cmd.none )

                Ok validCredentials ->
                    ( { model | formState = Loading }, Api.Signup.submitSignup validCredentials SignupDone )

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
                [ Html.text "Email"
                , Html.input
                    [ -- HA.class Gs.inputStyle
                      HA.type_ "text"
                    , HE.onInput StoreEmail
                    , HA.value model.storeEmail
                    ]
                    []
                ]
            , Html.div
                [--  HA.class [ Tw.flex, Tw.flex_col, Tw.gap_3 ]
                ]
                [ Html.text "Password"
                , Html.input
                    [ -- HA.class Gs.inputStyle
                      HA.type_ "password"
                    , HE.onInput StorePassword
                    , HA.value model.storePassword
                    ]
                    []
                ]
            , Html.div
                [--  HA.class [ Tw.flex, Tw.flex_col, Tw.gap_3 ]
                ]
                [ Html.text "Confirm Password"
                , Html.input
                    [ --  HA.class Gs.inputStyle
                      HA.type_ "password"
                    , HE.onInput StoreConfirmPassword
                    , HA.value model.storeConfirmPassword
                    ]
                    []
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
