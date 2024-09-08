module ForgotPassword exposing (..)

import Api.ForgotPassword
import Components.Error
import Data.User as User
import Data.Util as Util
import Html exposing (Html, text)
import Html.Attributes as HA
import Html.Events as HE
import Http


type alias Model =
    { storeEmail : String, formState : FormState }


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
    { storeEmail = "", formState = Initial }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    Html.div
        [-- HA.class [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_6, Tw.relative, Bp.sm [ Tw.m_20 ] ]
        ]
        [ Html.h2 [] [ text "Forgot Password" ]
        , case model.formState of
            Loading ->
                Html.div
                    [-- HA.class [ Tw.absolute, Tw.w_full, Tw.h_full, Tw.flex, Tw.justify_center, Tw.items_center, Tw.bg_color Tw.sky_50, Tw.bg_opacity_40 ]
                    ]
                    [ Util.loadingElement ]

            Error error ->
                Html.p
                    [-- HA.class [ Tw.text_color Tw.red_400 ]
                    ]
                    [ text error ]

            Initial ->
                text ""

            Success ->
                Html.div
                    [--  HA.class [ Tw.text_center ]
                    ]
                    [ Html.h2 [] [ text "Submitted successfully !" ]
                    , Html.p [] [ text "Check your email for rest link." ]
                    ]
        , Html.p
            []
            [ text "Enter your email and we will send you a reset link." ]
        , Html.form
            [-- HA.class [ Tw.flex, Tw.flex_col, Tw.gap_5, Tw.text_xl, Tw.w_full, Bp.md [ Tw.w_60 ] ]
            ]
            [ Html.input
                [ -- HA.class Gs.inputStyle,
                  HA.type_ "text"
                , HE.onInput StoreEmail
                , HA.value model.storeEmail
                ]
                []
            , Html.button
                [ -- HA.class Gs.buttonStyle
                  HA.type_ "button"
                , HE.onClick Submit
                ]
                [ text "Submit" ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit ->
            let
                validatedCred : Result String User.Email
                validatedCred =
                    User.fromStringToValidEmail model.storeEmail
            in
            case validatedCred of
                Err error ->
                    ( { model | formState = Error error }, Cmd.none )

                Ok validCredentials ->
                    ( { model | formState = Loading }
                    , Api.ForgotPassword.submitForgotPassword validCredentials Done
                    )

        StoreEmail str ->
            ( { model | storeEmail = str }, Cmd.none )

        Done (Ok _) ->
            ( { model | formState = Success, storeEmail = "" }, Cmd.none )

        Done (Err err) ->
            ( { model | formState = Error <| Components.Error.buildErrorMessage err }, Cmd.none )
