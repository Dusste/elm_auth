module Verification exposing (Model, Msg, init, update, view)

import Credentials
    exposing
        ( Session
        , Token
        , addHeader
        , decodeTokenData
        , encodeToken
        , fromSessionToToken
        , fromTokenToString
        , storeSession
        , tokenDecoder
        , verificationToString
        )
import Helpers exposing (loadingElement)
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr
import Http
import Json.Encode exposing (encode)
import Jwt
import Process
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
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
    = VerifyApiCallStart Session
    | VerifyDone (Result Http.Error Token)
    | TokenToLS Token



-- At this moment we are comfortable with VerificationParam being a string
-- since we are comparing it with verificatinstring from cookie


init : Session -> String -> ( Model, Cmd Msg )
init session verificationParam =
    case fromSessionToToken session of
        Just token ->
            case Jwt.decodeToken decodeTokenData <| fromTokenToString token of
                Ok resultTokenRecord ->
                    if verificationParam /= ("/verify-email/" ++ verificationToString resultTokenRecord.verificationstring) then
                        ( { userState = VerificationFail }, Cmd.none )

                    else if not resultTokenRecord.isverified then
                        ( { userState = VerificationPending }
                        , apiCallAfterSomeTime session VerifyApiCallStart
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


apiCallAfterSomeTime : Session -> (Session -> Msg) -> Cmd Msg
apiCallAfterSomeTime session toMsg =
    Process.sleep 5000
        |> Task.perform
            (\_ -> toMsg session)


apiCallToVerify : Session -> Cmd Msg
apiCallToVerify session =
    case fromSessionToToken session of
        Just token ->
            Http.request
                { method = "PUT"
                , headers = [ addHeader token ]
                , url = "/api/verify"
                , expect = Http.expectJson VerifyDone tokenDecoder
                , body = Http.emptyBody
                , timeout = Nothing
                , tracker = Nothing
                }

        Nothing ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        VerifyApiCallStart session ->
            ( model, apiCallToVerify session )

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
                    encodeToken token
            in
            ( model, storeSession <| Just <| encode 0 tokenValue )


view : Model -> Html Msg
view model =
    Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_6, Bp.sm [ Tw.m_20 ] ] ]
        [ case model.userState of
            VerificationPending ->
                Html.div []
                    [ Html.h2 [] [ text "Give us a moment to verify your account ! " ]
                    , Html.p [] [ text "Soon you will have access to a all profile features" ]
                    , loadingElement
                    ]

            VerificationDone ->
                Html.div []
                    [ Html.h2 [] [ text "Thanks for verifying your email ! " ]
                    , Html.p [] [ text "Now you will be redirected to your profile page and have full access to all app's features" ]
                    , loadingElement
                    ]

            VerificationFail ->
                Html.div []
                    [ Html.h2 [] [ text "UPS seems that something is off !" ]
                    , Html.p [] [ text "Try to re-login or refresh the page" ]
                    ]

            Verified ->
                Html.div []
                    [ Html.h2 [] [ text "HMMm seems that you're already verified !" ]
                    , Html.p [] [ text "Please proceed to you profile" ]
                    ]

            Sessionless ->
                Html.div []
                    [ Html.h2 [] [ text "You are not logged in !" ]
                    , Html.p [] [ text "Please proceed to login" ]
                    ]
        ]
