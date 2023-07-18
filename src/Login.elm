module Login exposing (Model, Msg, init, update, view)

import Credentials exposing (Token, encodeToken, storeSession, tokenDecoder)
import GlobalStyles as Gs
import Helpers exposing (buildErrorMessage, loadingElement)
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr exposing (type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Json.Encode exposing (encode)
import Process
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Task
import User


type alias Model =
    { storeEmail : String
    , storePassword : String
    , formState : FormState
    }


initialModel : Model
initialModel =
    { storeEmail = ""
    , storePassword = ""
    , formState = Initial
    }


type FormState
    = Initial
    | Loading
    | Error String


type Msg
    = StoreEmail String
    | StorePassword String
    | LoginSubmit
    | LoginDone (Result Http.Error Token)
    | HideError


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoreEmail email ->
            ( { model | storeEmail = email }, Cmd.none )

        StorePassword password ->
            ( { model | storePassword = password }, Cmd.none )

        LoginSubmit ->
            let
                validatedCred : Result String User.ValidCredentials
                validatedCred =
                    User.validateCredentials { email = model.storeEmail, password = model.storePassword }
            in
            case validatedCred of
                Err error ->
                    ( { model | formState = Error error }, Process.sleep 4000 |> Task.perform (\_ -> HideError) )

                Ok validCredentials ->
                    ( { model | formState = Loading }, submitLogin validCredentials )

        HideError ->
            ( { model | formState = Initial }, Cmd.none )

        LoginDone (Ok token) ->
            let
                tokenValue =
                    encodeToken token
            in
            ( { model | formState = Initial }, storeSession <| Just <| encode 0 tokenValue )

        LoginDone (Err error) ->
            ( { model | formState = Error <| buildErrorMessage error }, Process.sleep 4000 |> Task.perform (\_ -> HideError) )


submitLogin : User.ValidCredentials -> Cmd Msg
submitLogin credentials =
    Http.post
        { url = "/api/login"
        , body = Http.jsonBody (User.credentialsEncoder credentials)
        , expect = Http.expectJson LoginDone tokenDecoder
        }


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.css [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_6, Tw.relative, Bp.md [ Tw.m_20 ] ] ]
        [ Html.h2 [ Attr.css [ Tw.text_3xl ] ] [ text "Login" ]
        , case model.formState of
            Loading ->
                Html.div [ Attr.css [ Tw.absolute, Tw.w_full, Tw.h_full, Tw.flex, Tw.justify_center, Tw.items_center, Tw.bg_color Tw.sky_50, Tw.bg_opacity_40 ] ] [ loadingElement ]

            Error error ->
                Html.p [ Attr.css [ Tw.text_color Tw.red_400 ] ] [ text error ]

            Initial ->
                text ""
        , Html.form [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_5, Tw.text_xl, Tw.w_full, Bp.md [ Tw.w_60 ] ] ]
            [ Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_3 ] ]
                [ Html.label [] [ text "Email" ]
                , Html.input [ Attr.css Gs.inputStyle, type_ "text", onInput StoreEmail, value model.storeEmail ] []
                ]
            , Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_3 ] ]
                [ Html.label [ Attr.css [] ] [ text "Password" ]
                , Html.input [ Attr.css Gs.inputStyle, type_ "password", onInput StorePassword, value model.storePassword ] []
                ]
            , Html.button [ Attr.css Gs.buttonStyle, type_ "button", onClick LoginSubmit ] [ text "Sign in" ]
            , Html.a [ Attr.href "/forgot-password", Attr.css [ Tw.mt_5 ] ] [ text "Forgot password ?" ]
            ]
        , Html.a [ Attr.href "/forgot-password", Attr.css [ Tw.mt_5 ] ] [ text "Forgot password ?" ]
        ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )
