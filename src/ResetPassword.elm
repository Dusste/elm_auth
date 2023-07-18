module ResetPassword exposing (..)

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


type alias ResetCodeParam =
    String


type alias Model =
    { storePassword : String, storeConfirmPassword : String, formState : FormState, resetCodeParam : ResetCodeParam }


type Msg
    = StorePassword String
    | StoreConfirmPassword String
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
    { storePassword = "", storeConfirmPassword = "", formState = Initial, resetCodeParam = "" }


init : String -> ( Model, Cmd Msg )
init resetCodeParam =
    let
        cleanedResetCode =
            String.replace "/password-reset/" "" resetCodeParam
    in
    ( { initialModel | resetCodeParam = cleanedResetCode }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.css [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_6, Tw.relative, Bp.sm [ Tw.m_20 ] ] ]
        [ Html.h2 [] [ text "Reset password" ]
        , case model.formState of
            Loading ->
                Html.div [ Attr.css [ Tw.absolute, Tw.w_full, Tw.h_full, Tw.flex, Tw.justify_center, Tw.items_center, Tw.bg_color Tw.sky_50, Tw.bg_opacity_40 ] ] [ loadingElement ]

            Error error ->
                Html.p [ Attr.css [ Tw.text_color Tw.red_400 ] ] [ text error ]

            Initial ->
                text ""

            Success ->
                Html.div [ Attr.css [ Tw.text_center ] ]
                    [ Html.h2 [] [ text "All done !" ]
                    , Html.p [] [ text "Your password has been reset. Please login with your new password." ]
                    ]
        , Html.form [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_5, Tw.text_xl, Tw.w_full, Bp.md [ Tw.w_60 ] ] ]
            [ Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_3 ] ]
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
            , Html.button [ Attr.css Gs.buttonStyle, type_ "button", onClick Submit ] [ text "Submit" ]
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
                    ( { model | formState = Error error }, Process.sleep 4000 |> Task.perform (\_ -> HideError) )

                Ok { password } ->
                    ( { model | formState = Loading }, submitResetPassword password model.resetCodeParam )

        HideError ->
            ( { model | formState = Initial }, Cmd.none )

        StorePassword str ->
            ( { model | storePassword = str }, Cmd.none )

        StoreConfirmPassword str ->
            ( { model | storeConfirmPassword = str }, Cmd.none )

        Done (Ok _) ->
            ( { model | formState = Success, storePassword = "", storeConfirmPassword = "" }, Cmd.none )

        Done (Err err) ->
            ( { model | formState = Error <| buildErrorMessage err }, Process.sleep 4000 |> Task.perform (\_ -> HideError) )


submitResetPassword : User.Password -> ResetCodeParam -> Cmd Msg
submitResetPassword password resetCodeParam =
    Http.post
        { url = "/api/reset-password/" ++ resetCodeParam
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "password", User.passwordEncoder password ) ]
        , expect = Http.expectWhatever Done
        }
