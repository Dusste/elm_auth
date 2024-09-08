module Login exposing (Model, Msg, init, update, view)

import Api.Login
import Components.Element
import Components.Error
import Data.Credentials as Credentials
import Data.Ports as Ports
import Data.User as User
import Data.Util as Util
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Encode
import Maybe.Extra


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoreEmail email ->
            ( { model | storeEmail = email }, Cmd.none )

        StorePassword password ->
            ( { model | storePassword = password }, Cmd.none )

        LoginSubmit ->
            let
                errors : Dict String (List String)
                errors =
                    model.errors
                        |> Components.Error.updateError
                            (String.length model.storeEmail == 0)
                            "email"
                            "Email is empty"
                        |> Components.Error.updateError
                            (User.isEmailValid model.storeEmail
                                |> not
                            )
                            "email"
                            "Email is invalid"
                        |> Components.Error.updateError
                            (String.length model.storePassword == 0)
                            "password"
                            "Password cannot be empty"
            in
            if Components.Error.anyActiveError errors then
                ( { model | errors = errors }, Cmd.none )

            else
                ( { model | errors = errors }, Cmd.none )

        -- let
        --     validatedCred : Result String User.ValidCredentials
        --     validatedCred =
        --         User.validateCredentials { email = model.storeEmail, password = model.storePassword }
        -- in
        -- case validatedCred of
        --     Err error ->
        --         ( { model | formState = Error error }
        --         , Cmd.none
        --         )
        --     Ok validCredentials ->
        --         ( { model | formState = Loading }
        --         , Api.Login.submitLogin validCredentials LoginDone
        --         )
        LoginDone (Ok token) ->
            let
                tokenValue =
                    Credentials.encodeToken token
            in
            ( { model | formState = Initial }
            , Ports.storeSession <| Just <| Json.Encode.encode 0 tokenValue
            )

        LoginDone (Err error) ->
            ( { model | formState = Error <| Components.Error.buildErrorMessage error }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Html.div
        [ --  HA.class [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_6, Tw.relative, Bp.md [ Tw.m_20 ] ]
          HA.class "flex flex-col"
        ]
        [ Html.h2
            [-- HA.class [ Tw.text_3xl ]
            ]
            [ Html.text "Login" ]
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
                    [ Html.text error ]

            Initial ->
                Html.text ""
        , Html.form
            [--  HA.class [ Tw.flex, Tw.flex_col, Tw.gap_5, Tw.text_xl, Tw.w_full, Bp.md [ Tw.w_60 ] ]
            ]
            [ Html.div
                [-- HA.class [ Tw.flex, Tw.flex_col, Tw.gap_3 ]
                ]
                [ --  Html.label
                  --     []
                  --     [ Html.text "Email" ]
                  -- , Html.input
                  --     [ -- HA.class Gs.inputStyle
                  --       HA.type_ "text"
                  --     , HE.onInput StoreEmail
                  --     , HA.value model.storeEmail
                  --     ]
                  --     []
                  Components.Element.inputField
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

                -- Html.label
                --     [ HA.css [] ]
                --     [ text "Password" ]
                -- , Html.input
                --     [ HA.css Gs.inputStyle, type_ "password", onInput StorePassword, value model.storePassword ]
                --     []
                ]
            , Html.button
                [ -- HA.class Gs.buttonStyle
                  HA.type_ "button"
                , HE.onClick LoginSubmit
                ]
                [ Html.text "Sign in" ]
            , Html.a
                [ HA.href "/forgot-password"

                -- , HA.class [ Tw.mt_5 ]
                ]
                [ Html.text "Forgot password ?" ]
            ]
        ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )
