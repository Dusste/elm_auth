module Verification exposing (Model, Msg, init, update, view)

import Api.Verification
import Components.Element
import Components.Misc
import Data.Credentials as Credentials
import Data.OutMsg
import Data.Ports as Ports
import Html exposing (Html)
import Html.Attributes as HA
import Http
import Json.Encode
import Url


type alias Model =
    { userState : UserState
    , email : Maybe String
    , token : Credentials.Token
    }


type UserState
    = VerificationPending
    | VerificationFail
    | VerificationDone
    | Verified
    | Sessionless


type Msg
    = CheckVerify (Result Http.Error Credentials.Token)
    | Resend
    | RedirectToProfile


init : Credentials.Token -> Url.Url -> ( Model, Cmd Msg )
init token url =
    case Credentials.tokenToUserData token of
        Ok { isverified, verificationstring, email } ->
            if url.path /= ("/verify-email/" ++ verificationstring) then
                ( { userState = VerificationFail
                  , email = Just email
                  , token = token
                  }
                , Cmd.none
                )

            else if not isverified then
                ( { userState = VerificationPending
                  , email = Nothing
                  , token = token
                  }
                , Api.Verification.apiCallToVerify token CheckVerify
                )

            else
                ( { userState = Verified
                  , email = Nothing
                  , token = token
                  }
                , Cmd.none
                )

        Err _ ->
            ( { userState = Sessionless
              , email = Nothing
              , token = token
              }
            , Cmd.none
            )


update : Msg -> Model -> ( Model, List Data.OutMsg.OutMsg, Cmd Msg )
update msg model =
    case msg of
        CheckVerify result ->
            case result of
                Ok token ->
                    let
                        tokenValue =
                            Credentials.encodeToken token
                    in
                    ( { model
                        | userState = VerificationDone
                        , token = token
                      }
                    , []
                    , Ports.storeSession <| Just <| Json.Encode.encode 0 tokenValue
                    )

                Err _ ->
                    ( { model
                        | userState = VerificationFail
                      }
                    , []
                    , Cmd.none
                    )

        RedirectToProfile ->
            ( model
            , [ Data.OutMsg.RedirectToProfile model.token ]
            , Cmd.none
            )

        Resend ->
            case model.email of
                Just email ->
                    ( model
                    , [ Data.OutMsg.ResendEmail email model.token ]
                    , Cmd.none
                    )

                Nothing ->
                    ( model, [], Cmd.none )


view : Model -> Html Msg
view model =
    Html.div
        [ HA.class "flex flex-col items-center justify-center mt-64"
        ]
        [ case model.userState of
            VerificationPending ->
                layout
                    "Give us a moment to verify your account !"
                    "Soon you'll have access to all features"
                    Components.Misc.loadingElement

            VerificationDone ->
                layout
                    "Thanks for verifying your email ! "
                    "Now you'll be redirected to your profile page and have full access to all features"
                    Components.Misc.loadingElement

            VerificationFail ->
                layout
                    "UPS seems that something is off !"
                    "Try to re-login or refresh the page"
                    (Html.div
                        [ HA.class "mt-4" ]
                        [ Components.Element.button
                            |> Components.Element.withText "Resend email"
                            |> Components.Element.withMsg Resend
                            |> Components.Element.withDisabled False
                            |> Components.Element.withSecondaryStyle
                            |> Components.Element.toHtml
                        ]
                    )

            Verified ->
                layout
                    "HMMm seems that you're already verified !"
                    "Please proceed to you profile"
                    (Html.div
                        [ HA.class "mt-4" ]
                        [ Components.Element.button
                            |> Components.Element.withText "View profile"
                            |> Components.Element.withMsg RedirectToProfile
                            |> Components.Element.withDisabled False
                            |> Components.Element.withSecondaryStyle
                            |> Components.Element.toHtml
                        ]
                    )

            Sessionless ->
                layout
                    "You are not logged in !"
                    "Please proceed to login"
                    (Components.Element.button
                        |> Components.Element.withText "Resend email"
                        |> Components.Element.withMsg Resend
                        |> Components.Element.withDisabled False
                        |> Components.Element.withSecondaryStyle
                        |> Components.Element.toHtml
                    )
        ]


layout : String -> String -> Html Msg -> Html Msg
layout head description element =
    Html.div
        [ HA.class "flex flex-col items-center gap-y-4" ]
        [ Html.h2
            []
            [ Html.text head ]
        , Components.Element.notification (Components.Element.Info description)
        , element
        ]
