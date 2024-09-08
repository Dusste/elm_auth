module Signup exposing (Model, Msg, init, update, view)

import Api.Signup
import Data.Credentials as Credentials
import Data.Ports as Ports
import Data.User as User
import Data.Util as Util
import GlobalStyles as Gs
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr exposing (type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Json.Encode
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw


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
            ( { model | formState = Error <| Util.buildErrorMessage error }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.css [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_6, Tw.relative, Bp.md [ Tw.m_20 ] ] ]
        [ Html.h2 [ Attr.css [ Tw.text_3xl ] ] [ text "Signup" ]
        , case model.formState of
            Loading ->
                Html.div [ Attr.css [ Tw.absolute, Tw.w_full, Tw.h_full, Tw.flex, Tw.justify_center, Tw.items_center, Tw.bg_color Tw.sky_50, Tw.bg_opacity_40 ] ] [ Util.loadingElement ]

            Error error ->
                Html.p [ Attr.css [ Tw.text_color Tw.red_400 ] ] [ text error ]

            Initial ->
                text ""
        , Html.form [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_5, Tw.text_xl, Tw.w_full, Bp.md [ Tw.w_60 ] ] ]
            [ Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_3 ] ]
                [ text "Email"
                , Html.input
                    [ Attr.css Gs.inputStyle
                    , type_ "text"
                    , onInput StoreEmail
                    , value model.storeEmail
                    ]
                    []
                ]
            , Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_3 ] ]
                [ text "Password"
                , Html.input
                    [ Attr.css Gs.inputStyle
                    , type_ "password"
                    , onInput StorePassword
                    , value model.storePassword
                    ]
                    []
                ]
            , Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_3 ] ]
                [ text "Confirm Password"
                , Html.input
                    [ Attr.css Gs.inputStyle
                    , type_ "password"
                    , onInput StoreConfirmPassword
                    , value model.storeConfirmPassword
                    ]
                    []
                ]
            , Html.button [ Attr.css Gs.buttonStyle, type_ "button", onClick SignupSubmit ] [ text "Sign up" ]
            ]
        ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )
