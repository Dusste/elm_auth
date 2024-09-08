module ResetPassword exposing (..)

import Api.ResetPassword
import Components.Error
import Data.User as User
import Data.Util as Util
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http


type alias Model =
    { storePassword : String
    , storeConfirmPassword : String
    , formState : FormState
    , resetCodeParam : String
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
    }


init : String -> ( Model, Cmd Msg )
init resetCodeParam =
    ( { initialModel | resetCodeParam = resetCodeParam }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    Html.div
        [-- HA.class[ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_6, Tw.relative, Bp.sm [ Tw.m_20 ] ]
        ]
        [ Html.h2
            []
            [ Html.text "Reset password" ]
        , case model.formState of
            Loading ->
                Html.div
                    [--  HA.class [ Tw.absolute, Tw.w_full, Tw.h_full, Tw.flex, Tw.justify_center, Tw.items_center, Tw.bg_color Tw.sky_50, Tw.bg_opacity_40 ]
                    ]
                    [ Util.loadingElement ]

            Error error ->
                Html.p
                    [-- HA.class [ Tw.text_color Tw.red_400 ]
                    ]
                    [ Html.text error ]

            Initial ->
                Html.text ""

            Success ->
                Html.div
                    [-- HA.class [ Tw.text_center ]
                    ]
                    [ Html.h2
                        []
                        [ Html.text "All done !" ]
                    , Html.p
                        []
                        [ Html.text "Your password has been reset. Please login with your new password." ]
                    ]
        , Html.form
            [-- HA.class [ Tw.flex, Tw.flex_col, Tw.gap_5, Tw.text_xl, Tw.w_full, Bp.md [ Tw.w_60 ] ]
            ]
            [ Html.div
                [-- HA.class [ Tw.flex, Tw.flex_col, Tw.gap_3 ]
                ]
                [ Html.text "Password"
                , Html.input
                    [ --  HA.class Gs.inputStyle
                      HA.type_ "password"
                    , HE.onInput StorePassword
                    , HA.value model.storePassword
                    ]
                    []
                ]
            , Html.div
                [-- HA.class [ Tw.flex, Tw.flex_col, Tw.gap_3 ]
                ]
                [ Html.text "Confirm Password"
                , Html.input
                    [ -- HA.class Gs.inputStyle
                      HA.type_ "password"
                    , HE.onInput StoreConfirmPassword
                    , HA.value model.storeConfirmPassword
                    ]
                    []
                ]
            , Html.button
                [ -- HA.class Gs.buttonStyle
                  HA.type_ "button"
                , HE.onClick Submit
                ]
                [ Html.text "Submit" ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit ->
            let
                validatedCred : Result String { password : User.Password }
                validatedCred =
                    User.validateConfirmPassword { password = model.storePassword, confirmPassword = model.storeConfirmPassword }
            in
            case validatedCred of
                Err error ->
                    ( { model | formState = Error error }, Cmd.none )

                Ok { password } ->
                    ( { model | formState = Loading }
                    , Api.ResetPassword.submitResetPassword password model.resetCodeParam Done
                    )

        StorePassword str ->
            ( { model | storePassword = str }, Cmd.none )

        StoreConfirmPassword str ->
            ( { model | storeConfirmPassword = str }, Cmd.none )

        Done (Ok _) ->
            ( { model | formState = Success, storePassword = "", storeConfirmPassword = "" }, Cmd.none )

        Done (Err err) ->
            ( { model | formState = Error <| Components.Error.buildErrorMessage err }, Cmd.none )
