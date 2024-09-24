module Verification exposing (Model, Msg, init, update, view)

import Api.Verification
import Components.Element
import Components.Misc
import Data.Credentials as Credentials
import Data.OutMsg
import Data.Ports as Ports
import Data.Verification as Verification
import Html exposing (Html)
import Html.Attributes as HA
import Http
import Json.Encode
import Process
import Task


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
    = VerifyApiCallStart Credentials.Token
    | VerifyDone (Result Http.Error Credentials.Token)
    | TokenToLS Credentials.Token
    | Resend



-- At this moment we are comfortable with VerificationParam being a string
-- since we are comparing it with verificatinstring from cookie


init : Credentials.Token -> String -> ( Model, Cmd Msg )
init token verificationParam =
    case Credentials.tokenToUserData token of
        Ok { isverified, verificationstring, email } ->
            if verificationParam /= ("/verify-email/" ++ verificationstring) then
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
                , Components.Misc.sleepForAWhileThenCall token VerifyApiCallStart
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
        VerifyApiCallStart token ->
            ( model
            , []
            , Api.Verification.apiCallToVerify token VerifyDone
            )

        VerifyDone (Ok token) ->
            ( { model | userState = VerificationDone }
            , []
            , Process.sleep 5000
                |> Task.perform (\_ -> TokenToLS token)
            )

        VerifyDone (Err _) ->
            ( { model
                | userState = VerificationFail
              }
            , []
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

        TokenToLS token ->
            let
                tokenValue =
                    Credentials.encodeToken token
            in
            ( model
            , []
            , Ports.storeSession <| Just <| Json.Encode.encode 0 tokenValue
            )


view : Model -> Html Msg
view model =
    Html.div
        [ HA.class "flex flex-col items-center justify-center mt-64"
        ]
        [ case model.userState of
            VerificationPending ->
                Html.div
                    []
                    [ Html.h2
                        []
                        [ Html.text "Give us a moment to verify your account ! " ]
                    , Html.p
                        []
                        [ Html.text "Soon you will have access to a all profile features" ]
                    , Components.Misc.loadingElement
                    ]

            VerificationDone ->
                Html.div
                    []
                    [ Html.h2
                        []
                        [ Html.text "Thanks for verifying your email ! " ]
                    , Html.p
                        []
                        [ Html.text "Now you will be redirected to your profile page and have full access to all app's features" ]
                    , Components.Misc.loadingElement
                    ]

            VerificationFail ->
                Html.div
                    []
                    [ Html.h2
                        []
                        [ Html.text "UPS seems that something is off !" ]
                    , Html.p
                        []
                        [ Html.text "Try to re-login or refresh the page" ]
                    , Html.div
                        [ HA.class "mt-4" ]
                        [ Components.Element.button
                            |> Components.Element.withText "Resend email"
                            |> Components.Element.withMsg Resend
                            |> Components.Element.withDisabled False
                            |> Components.Element.withSecondaryStyle
                            |> Components.Element.toHtml
                        ]
                    ]

            Verified ->
                Html.div
                    []
                    [ Html.h2
                        []
                        [ Html.text "HMMm seems that you're already verified !" ]
                    , Html.p
                        []
                        [ Html.text "Please proceed to you profile" ]
                    ]

            Sessionless ->
                Html.div
                    []
                    [ Html.h2
                        []
                        [ Html.text "You are not logged in !" ]
                    , Html.p
                        []
                        [ Html.text "Please proceed to login" ]
                    ]
        ]
