module Verification exposing (Model, Msg, init, update, view)

import Api.Verification
import Components.Misc
import Data.Credentials as Credentials
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
    }


type UserState
    = VerificationPending
    | VerificationFail
    | VerificationDone
    | Verified
    | Sessionless


type Msg
    = VerifyApiCallStart Credentials.Session
    | VerifyDone (Result Http.Error Credentials.Token)
    | TokenToLS Credentials.Token



-- At this moment we are comfortable with VerificationParam being a string
-- since we are comparing it with verificatinstring from cookie


init : Credentials.Session -> String -> ( Model, Cmd Msg )
init session verificationParam =
    case Credentials.fromSessionToToken session of
        Just token ->
            case Credentials.tokenToUserData token of
                Ok resultTokenRecord ->
                    if verificationParam /= ("/verify-email/" ++ Verification.verificationToString resultTokenRecord.verificationstring) then
                        ( { userState = VerificationFail }, Cmd.none )

                    else if not resultTokenRecord.isverified then
                        ( { userState = VerificationPending }
                        , Components.Misc.sleepForAWhileThenCall session VerifyApiCallStart
                        )

                    else
                        ( { userState = Verified }, Cmd.none )

                Err _ ->
                    ( { userState = Sessionless }, Cmd.none )

        Nothing ->
            ( { userState = Sessionless
              }
            , Cmd.none
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        VerifyApiCallStart session ->
            ( model, Api.Verification.apiCallToVerify session VerifyDone )

        VerifyDone (Ok token) ->
            ( { model | userState = VerificationDone }
            , Process.sleep 5000
                |> Task.perform (\_ -> TokenToLS token)
            )

        VerifyDone (Err _) ->
            ( { model
                | userState = VerificationFail
              }
            , Cmd.none
            )

        TokenToLS token ->
            let
                tokenValue =
                    Credentials.encodeToken token
            in
            ( model, Ports.storeSession <| Just <| Json.Encode.encode 0 tokenValue )


view : Model -> Html Msg
view model =
    Html.div
        [-- HA.class [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_6, Bp.sm [ Tw.m_20 ] ]
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
