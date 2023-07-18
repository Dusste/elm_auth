module ForgotPassword exposing (..)

import GlobalStyles as Gs
import Helpers exposing (buildErrorMessage, loadingElement)
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr exposing (type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Json.Encode as Encode
import Process
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Task
import User


type alias Model =
    { storeEmail : String, formState : FormState }


type Msg
    = StoreEmail String
    | Submit
    | HideError
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
        [ Attr.css [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_6, Tw.relative, Bp.sm [ Tw.m_20 ] ] ]
        [ Html.h2 [] [ text "Forgot Password" ]
        , case model.formState of
            Loading ->
                Html.div [ Attr.css [ Tw.absolute, Tw.w_full, Tw.h_full, Tw.flex, Tw.justify_center, Tw.items_center, Tw.bg_color Tw.sky_50, Tw.bg_opacity_40 ] ] [ loadingElement ]

            Error error ->
                Html.p [ Attr.css [ Tw.text_color Tw.red_400 ] ] [ text error ]

            Initial ->
                text ""

            Success ->
                Html.div [ Attr.css [ Tw.text_center ] ]
                    [ Html.h2 [] [ text "Submitted successfully !" ]
                    , Html.p [] [ text "Check your email for rest link." ]
                    ]
        , Html.p [] [ text "Enter your email and we will send you a reset link." ]
        , Html.form [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_5, Tw.text_xl, Tw.w_full, Bp.md [ Tw.w_60 ] ] ]
            [ Html.input [ Attr.css Gs.inputStyle, type_ "text", onInput StoreEmail, value model.storeEmail ] []
            , Html.button [ Attr.css Gs.buttonStyle, type_ "button", onClick Submit ] [ text "Submit" ]
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
                    ( { model | formState = Error error }, Process.sleep 4000 |> Task.perform (\_ -> HideError) )

                Ok validCredentials ->
                    ( { model | formState = Loading }, submitForgotPassword validCredentials )

        HideError ->
            ( { model | formState = Initial }, Cmd.none )

        StoreEmail str ->
            ( { model | storeEmail = str }, Cmd.none )

        Done (Ok _) ->
            ( { model | formState = Success, storeEmail = "" }, Cmd.none )

        Done (Err err) ->
            ( { model | formState = Error <| buildErrorMessage err }, Process.sleep 4000 |> Task.perform (\_ -> HideError) )


submitForgotPassword : User.Email -> Cmd Msg
submitForgotPassword email =
    Http.post
        { url = "/api/forgot-password"
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "email", User.emailEncoder email ) ]
        , expect = Http.expectWhatever Done
        }
