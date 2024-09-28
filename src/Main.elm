module Main exposing (..)

import Api.Verification
import Browser
import Browser.Navigation as Nav
import Components.SvgIcon
import Data.Credentials as Credentials
import Data.OutMsg
import Data.Ports as Ports
import Data.Verification as Verification
import ForgotPassword
import Home
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode
import Jwt
import Login
import Profile
import ResetPassword
import Signup
import Task
import Time
import Url exposing (Url)
import Url.Parser exposing ((</>))
import Verification


type alias Model =
    { page : Page
    , url : Url
    , key : Nav.Key
    , session : Credentials.Session
    , openDropdown : Bool
    , time : Maybe Time.Posix
    }


type Page
    = LoginPage Login.Model
    | SignupPage Signup.Model
    | ProfilePage Profile.Model
    | ForgotPasswordPage ForgotPassword.Model
    | HomePage Home.Model
    | VerificationPage Verification.Model
    | ResetPasswordPage ResetPassword.Model
    | NotFoundPage


type Route
    = Login
    | Signup
    | Profile String
    | ForgotPassword
    | ResetPassword String
    | Home
    | Verification String
    | NotFound


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | GotSignupMsg Signup.Msg
    | GotLoginMsg Login.Msg
    | GotProfileMsg Profile.Msg
    | GotFpMsg ForgotPassword.Msg
    | GotRpMsg ResetPassword.Msg
    | GotHomeMsg Home.Msg
    | NoOp
    | GotVerificationMsg Verification.Msg
    | GotSubscriptionChangeMsg Credentials.Session
    | GetLogout
    | OpenDropdown
    | GotTime Time.Posix
    | CheckSessionExpired ( Credentials.Session, Maybe Time.Posix )
    | RequestToReSendEmail String Credentials.Token
    | ResendDone (Result Http.Error ())


content : Model -> Html Msg
content model =
    case model.page of
        LoginPage loginModel ->
            Login.view loginModel
                |> Html.map GotLoginMsg

        SignupPage signupModel ->
            Signup.view signupModel
                |> Html.map GotSignupMsg

        ProfilePage profileModel ->
            Profile.view profileModel
                |> Html.map GotProfileMsg

        ForgotPasswordPage fpModal ->
            ForgotPassword.view fpModal
                |> Html.map GotFpMsg

        ResetPasswordPage rPasswordModel ->
            ResetPassword.view rPasswordModel
                |> Html.map GotRpMsg

        HomePage _ ->
            Home.view
                |> Html.map GotHomeMsg

        VerificationPage verificationModel ->
            Verification.view verificationModel
                |> Html.map GotVerificationMsg

        NotFoundPage ->
            Html.p [] [ Html.text "Page not found buddy -_- sorry" ]


app : Model -> Html Msg
app model =
    Html.div
        [ HA.class "w-[1500px] m-auto"
        , HE.onClick <| CheckSessionExpired ( model.session, model.time )
        ]
        [ viewHeader model
        , content model

        -- , viewFooter @TODO add when its necessery
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Kickoff project"
    , body =
        [ app model ]
    }


viewFooter : Html Msg
viewFooter =
    Html.footer
        []
        [ Html.text "This is footer" ]


viewHeader : Model -> Html Msg
viewHeader { page, session, openDropdown } =
    Html.nav
        [ HA.class "flex mt-4 justify-between items-center"
        ]
        [ Html.h1
            []
            [ Html.a
                [ HA.href "/" ]
                [ Html.text "Kickoff project" ]
            ]
        , Html.div
            [ HA.class "flex justify-end" ]
            [ case Credentials.fromSessionToToken session of
                Just token ->
                    viewPrivateHeader { page = page, token = token, openDropdown = openDropdown }

                Nothing ->
                    viewPublicHeader page
            ]
        ]


viewProfilePic : Maybe String -> List (Html.Attribute msg) -> Html msg
viewProfilePic maybeSrc attr =
    case maybeSrc of
        Just url ->
            Html.img
                (List.append
                    [ HA.src url
                    ]
                    attr
                )
                []

        Nothing ->
            Html.text ""


viewPublicHeader : Page -> Html Msg
viewPublicHeader page =
    Html.ul
        [ HA.class "flex justify-between gap-4"
        ]
        [ Html.li
            [ HA.classList
                [ ( "active"
                  , isActive { link = Home, page = page }
                  )
                ]
            ]
            [ Html.a
                [ HA.href "/"
                ]
                [ Html.text "home" ]
            ]
        , Html.li
            [ HA.classList
                [ ( "active"
                  , isActive { link = Login, page = page }
                  )
                ]
            ]
            [ Html.a
                [ HA.href "/login"
                ]
                [ Html.text "login" ]
            ]
        , Html.li
            [ HA.classList
                [ ( "active"
                  , isActive { link = Signup, page = page }
                  )
                ]
            ]
            [ Html.a
                [ HA.href "/signup"
                ]
                [ Html.text "signup" ]
            ]
        ]


viewPrivateHeader : { page : Page, token : Credentials.Token, openDropdown : Bool } -> Html Msg
viewPrivateHeader { page, token, openDropdown } =
    Html.ul
        [ HA.class "flex justify-between gap-4 items-end items-center"
        ]
        [ case Credentials.tokenToUserData token of
            Ok resultTokenRecord ->
                Html.li
                    [ HA.class "cursor-pointer"
                    ]
                    [ Html.div
                        [ HA.class "relative"
                        ]
                        [ Html.div
                            [ HA.class "flex items-center gap-x-2"
                            , HE.onClick OpenDropdown
                            ]
                            [ Html.div
                                [ HA.class "w-10 h-10 overflow-hidden rounded-full"
                                ]
                                [ viewProfilePic resultTokenRecord.profilepicurl
                                    [ HA.class "w-10" ]
                                ]
                            , Html.span
                                []
                                [ Html.text <|
                                    if String.length resultTokenRecord.firstname > 0 then
                                        resultTokenRecord.firstname

                                    else
                                        resultTokenRecord.email
                                ]
                            , Html.span
                                [ HA.class <|
                                    "flex transition-all w-[12px] "
                                        ++ (if openDropdown then
                                                "rotate-180"

                                            else
                                                ""
                                           )
                                ]
                                [ Components.SvgIcon.iconArrowDown ]
                            ]
                        , Html.ul
                            [ HA.class "flex absolute mt-3 flex-col gap-1 overflow-hidden duration-500 bg-color white"
                            , HA.style
                                "height"
                                (if openDropdown then
                                    "fit-content"

                                 else
                                    "0"
                                )
                            ]
                            [ Html.li
                                [ HA.classList
                                    [ ( "active"
                                      , isActive { link = Profile resultTokenRecord.id, page = page }
                                      )
                                    ]
                                , HE.onClick OpenDropdown
                                ]
                                [ Html.a
                                    [ HA.href <| "/profile/" ++ resultTokenRecord.id
                                    ]
                                    [ Html.text "My profile" ]
                                ]
                            , Html.li
                                [ HE.onClick OpenDropdown ]
                                [ Html.a
                                    []
                                    [ Html.text "option2" ]
                                ]
                            , Html.li
                                [ HE.onClick OpenDropdown ]
                                [ Html.a
                                    []
                                    [ Html.text "option3" ]
                                ]
                            , Html.li
                                []
                                [ Html.a
                                    [ HA.href "/"
                                    , HE.onClick GetLogout
                                    ]
                                    [ Html.text "logout" ]
                                ]
                            ]
                        ]
                    ]

            Err _ ->
                Html.text ""
        ]


isActive : { link : Route, page : Page } -> Bool
isActive { link, page } =
    case ( link, page ) of
        ( Login, LoginPage _ ) ->
            True

        ( Login, _ ) ->
            False

        ( Signup, SignupPage _ ) ->
            True

        ( Signup, _ ) ->
            False

        ( Profile _, ProfilePage _ ) ->
            True

        ( ForgotPassword, ForgotPasswordPage _ ) ->
            True

        ( ForgotPassword, _ ) ->
            False

        ( ResetPassword _, ResetPasswordPage _ ) ->
            True

        ( ResetPassword _, _ ) ->
            False

        ( Profile _, _ ) ->
            False

        ( Home, HomePage _ ) ->
            True

        ( Home, _ ) ->
            False

        ( Verification _, VerificationPage _ ) ->
            True

        ( Verification _, _ ) ->
            False

        ( NotFound, _ ) ->
            False


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

        ChangedUrl url ->
            let
                newPage : Page
                newPage =
                    urlToPage url model.session
            in
            initCurrentPage ( url, { model | page = newPage }, Cmd.none )

        GotLoginMsg loginMsg ->
            case model.page of
                LoginPage loginModel ->
                    let
                        ( loginModelFromLogin, outMsgs, loginMsgFromLogin ) =
                            Login.update loginMsg loginModel
                    in
                    ( { model | page = LoginPage loginModelFromLogin }
                    , Cmd.batch
                        [ Cmd.map GotLoginMsg loginMsgFromLogin
                        , Data.OutMsg.msgToCmd (outMsgToMsg outMsgs model.url)
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        GotProfileMsg profileMsg ->
            case model.page of
                ProfilePage profileModel ->
                    let
                        ( profileModelFromProfile, outMsgs, profileMsgFromProfile ) =
                            Profile.update profileMsg profileModel
                    in
                    ( { model | page = ProfilePage profileModelFromProfile }
                    , Cmd.batch
                        [ Cmd.map GotProfileMsg profileMsgFromProfile
                        , Data.OutMsg.msgToCmd (outMsgToMsg outMsgs model.url)
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        GotHomeMsg homeMsg ->
            case model.page of
                HomePage homeModel ->
                    let
                        ( homeModelFromHome, homeMsgFromHome ) =
                            Home.update homeMsg homeModel
                    in
                    ( { model | page = HomePage homeModelFromHome }, Cmd.map GotHomeMsg homeMsgFromHome )

                _ ->
                    ( model, Cmd.none )

        GotVerificationMsg verificationMsg ->
            case model.page of
                VerificationPage verificationModel ->
                    let
                        ( verificationModelFromVerification, outMsgs, verificationMsgFromVerification ) =
                            Verification.update verificationMsg verificationModel
                    in
                    ( { model | page = VerificationPage verificationModelFromVerification }
                    , Cmd.batch
                        [ Cmd.map GotVerificationMsg verificationMsgFromVerification
                        , Data.OutMsg.msgToCmd (outMsgToMsg outMsgs model.url)
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        GotRpMsg rpMsg ->
            case model.page of
                ResetPasswordPage rpModel ->
                    let
                        ( rpModelFromRp, rpMsgFromRp ) =
                            ResetPassword.update rpMsg rpModel
                    in
                    ( { model | page = ResetPasswordPage rpModelFromRp }, Cmd.map GotRpMsg rpMsgFromRp )

                _ ->
                    ( model, Cmd.none )

        GotFpMsg fpMsg ->
            case model.page of
                ForgotPasswordPage fpModel ->
                    let
                        ( modelFromFp, msgFromFp ) =
                            ForgotPassword.update fpMsg fpModel
                    in
                    ( { model | page = ForgotPasswordPage modelFromFp }, Cmd.map GotFpMsg msgFromFp )

                _ ->
                    ( model, Cmd.none )

        GotSignupMsg signupMsg ->
            case model.page of
                SignupPage signupModel ->
                    let
                        ( signupModelFromSignup, signupMsgFromSignup ) =
                            Signup.update signupMsg signupModel
                    in
                    ( { model | page = SignupPage signupModelFromSignup }, Cmd.map GotSignupMsg signupMsgFromSignup )

                _ ->
                    ( model, Cmd.none )

        GotSubscriptionChangeMsg session ->
            ( { model | session = session }
            , case Credentials.fromSessionToToken session of
                Just token ->
                    case Credentials.tokenToUserData token of
                        Ok resultTokenRecord ->
                            Nav.pushUrl model.key ("/profile/" ++ resultTokenRecord.id)

                        Err _ ->
                            Nav.pushUrl model.key "/login"

                Nothing ->
                    Nav.pushUrl model.key "/login"
            )

        CheckSessionExpired ( session, maybeTime ) ->
            ( model, handleLogout session maybeTime )

        GetLogout ->
            ( model, Ports.logout )

        GotTime time ->
            ( { model | time = Just time }, Cmd.none )

        OpenDropdown ->
            ( { model | openDropdown = not model.openDropdown }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        RequestToReSendEmail email token ->
            ( model, Api.Verification.requestResendEmail email token ResendDone )

        ResendDone result ->
            case result of
                Ok _ ->
                    ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


handleLogout : Credentials.Session -> Maybe Time.Posix -> Cmd Msg
handleLogout session maybeTime =
    case ( Credentials.fromSessionToToken session, maybeTime ) of
        ( Just token, Just time ) ->
            let
                tokenString =
                    Credentials.fromTokenToString token
            in
            case Jwt.isExpired time tokenString of
                Ok isExpired ->
                    if isExpired then
                        Ports.logout

                    else
                        Cmd.none

                Err _ ->
                    Cmd.none

        _ ->
            Cmd.none


matchRoute : Url.Parser.Parser (Route -> a) a
matchRoute =
    Url.Parser.oneOf
        [ Url.Parser.map Home Url.Parser.top
        , Url.Parser.map ForgotPassword (Url.Parser.s "forgot-password")
        , Url.Parser.map Login (Url.Parser.s "login")
        , Url.Parser.map Profile (Url.Parser.s "profile" </> Url.Parser.string)
        , Url.Parser.map Signup (Url.Parser.s "signup")
        , Url.Parser.map Verification (Url.Parser.s "verify-email" </> Url.Parser.string)
        , Url.Parser.map ResetPassword (Url.Parser.s "reset-password" </> Url.Parser.string)
        ]


urlToPage : Url -> Credentials.Session -> Page
urlToPage url session =
    case ( Url.Parser.parse matchRoute url, Credentials.fromSessionToToken session ) of
        ( Just Login, Nothing ) ->
            LoginPage (Tuple.first (Login.init ()))

        ( Just Signup, Nothing ) ->
            SignupPage (Tuple.first (Signup.init ()))

        ( Just (Profile _), Just token ) ->
            ProfilePage (Tuple.first (Profile.init token))

        ( Just (Verification _), Just token ) ->
            VerificationPage (Tuple.first (Verification.init token url))

        ( Just (ResetPassword _), Nothing ) ->
            let
                cleanedResetCode =
                    String.replace "/password-reset/" "" url.path
            in
            ResetPasswordPage (Tuple.first (ResetPassword.init cleanedResetCode))

        ( Just ForgotPassword, Nothing ) ->
            ForgotPasswordPage (Tuple.first (ForgotPassword.init ()))

        ( Just Home, _ ) ->
            HomePage (Tuple.first (Home.init ()))

        _ ->
            NotFoundPage


initCurrentPage : ( Url, Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( url, model, existingCmds ) =
    case model.page of
        LoginPage _ ->
            let
                ( pageModel, pageCmds ) =
                    Login.init ()

                -- Because Main doesn’t know anything about the page specific messages, it needs to map them to one of the data constructors from its own Msg type using the Cmd.map function
            in
            ( { model | page = LoginPage pageModel }
            , Cmd.map GotLoginMsg pageCmds
            )

        SignupPage _ ->
            let
                ( pageModel, pageCmds ) =
                    Signup.init ()
            in
            ( { model | page = SignupPage pageModel }
            , Cmd.map GotSignupMsg pageCmds
            )

        HomePage _ ->
            let
                ( pageModel, pageCmds ) =
                    Home.init ()
            in
            ( { model | page = HomePage pageModel }
            , Cmd.batch [ Cmd.map GotHomeMsg pageCmds, existingCmds ]
            )

        ForgotPasswordPage _ ->
            let
                ( pageModel, pageCmds ) =
                    ForgotPassword.init ()
            in
            ( { model | page = ForgotPasswordPage pageModel }
            , Cmd.batch [ Cmd.map GotFpMsg pageCmds, existingCmds ]
            )

        ResetPasswordPage _ ->
            let
                ( pageModel, pageCmds ) =
                    ResetPassword.init url.path
            in
            ( { model | page = ResetPasswordPage pageModel }
            , Cmd.map GotRpMsg pageCmds
            )

        VerificationPage _ ->
            case Credentials.fromSessionToToken model.session of
                Just token ->
                    let
                        ( pageModel, pageCmds ) =
                            Verification.init token url
                    in
                    ( { model | page = VerificationPage pageModel }
                    , Cmd.map GotVerificationMsg pageCmds
                    )

                Nothing ->
                    ( model, Cmd.none )

        ProfilePage _ ->
            case Credentials.fromSessionToToken model.session of
                Just token ->
                    let
                        ( pageModel, pageCmds ) =
                            Profile.init token
                    in
                    ( { model | page = ProfilePage pageModel }
                    , Cmd.batch [ Cmd.map GotProfileMsg pageCmds, existingCmds ]
                    )

                Nothing ->
                    ( model, Cmd.none )

        NotFoundPage ->
            ( { model | page = NotFoundPage }, Cmd.none )


outMsgToMsg : List Data.OutMsg.OutMsg -> Url -> List Msg
outMsgToMsg outMsgs url =
    List.map
        (\outMsg ->
            case outMsg of
                Data.OutMsg.RedirectToProfile token ->
                    let
                        maybeId =
                            Credentials.tokenToId token
                    in
                    case maybeId of
                        Just id ->
                            ChangedUrl
                                { url
                                    | path = "/profile/" ++ id
                                }

                        Nothing ->
                            NoOp

                Data.OutMsg.ResendEmail email token ->
                    RequestToReSendEmail email token
        )
        outMsgs


init : Json.Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        session =
            Credentials.decodeToSession key flags

        model =
            { page = urlToPage url session
            , url = url
            , key = key
            , session = session
            , openDropdown = False
            , time = Nothing
            }
    in
    initCurrentPage ( url, model, Task.perform GotTime Time.now )


subscriptions : Model -> Sub Msg
subscriptions model =
    Credentials.subscriptionChanges GotSubscriptionChangeMsg model.key


main : Program Json.Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
