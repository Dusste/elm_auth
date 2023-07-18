module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Credentials
    exposing
        ( ResetCodeParam
        , Session
        , UserId
        , VerificationString
        , decodeToSession
        , decodeTokenData
        , fromSessionToToken
        , fromTokenToString
        , logout
        , passwordCodeStringParser
        , subscriptionChanges
        , userIdParser
        , userIdToString
        , verifictionStringParser
        )
import ForgotPassword
import Home
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr exposing (classList, href, src, style, width)
import Html.Styled.Events exposing (onClick)
import Json.Decode exposing (Value)
import Jwt
import Login
import Profile
import ResetPassword
import Signup
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Task
import Time
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s)
import Verification


type alias Model =
    { page : Page
    , key : Nav.Key
    , session : Session
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
    | Profile UserId
    | ForgotPassword
    | ResetPassword ResetCodeParam
    | Home
    | Verification VerificationString
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
    | GotVerificationMsg Verification.Msg
    | GotSubscriptionChangeMsg Session
    | GetLogout
    | OpenDropdown
    | GotTime Time.Posix
    | CheckSessionExpired ( Session, Maybe Time.Posix )


content : Model -> Html Msg
content model =
    Html.div []
        [ case model.page of
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
                Html.p [] [ text "Page not found buddy -_- sorry" ]
        ]


app : Model -> Html Msg
app model =
    Html.div [ onClick <| CheckSessionExpired ( model.session, model.time ) ]
        [ viewHeader model
        , content model

        -- , viewFooter @TODO add when its necessery
        ]


view : Model -> Document Msg
view model =
    { title = "My elm app"
    , body =
        [ Html.toUnstyled <| app model ]
    }


viewFooter : Html Msg
viewFooter =
    Html.footer [ Attr.css [ Tw.bg_color Tw.black, Tw.text_color Tw.white, Tw.p_10, Tw.w_full ] ]
        [ text "This is footer"
        ]


viewHeader : Model -> Html Msg
viewHeader { page, session, openDropdown } =
    Html.nav [ Attr.css [ Tw.flex, Tw.p_5, Tw.justify_between, Tw.items_center ] ]
        [ Html.h1 [] [ Html.a [ href "/" ] [ text "My elm app" ] ]
        , case fromSessionToToken session of
            Just token ->
                viewLoggedInHeader { page = page, token = token, openDropdown = openDropdown }

            Nothing ->
                Html.ul [ Attr.css [ Tw.flex, Tw.justify_between, Tw.gap_4 ] ]
                    [ Html.li
                        [ classList
                            [ ( "active"
                              , isActive { link = Home, page = page }
                              )
                            ]
                        ]
                        [ Html.a [ Attr.css [ Tw.py_1, Tw.px_4, Tw.text_xl, Tw.rounded, Tw.flex ], href "/" ] [ text "home" ] ]
                    , Html.li
                        [ classList
                            [ ( "active"
                              , isActive { link = Login, page = page }
                              )
                            ]
                        ]
                        [ Html.a [ Attr.css [ Tw.py_1, Tw.px_4, Tw.text_xl, Tw.rounded, Tw.flex ], href "/login" ] [ text "login" ] ]
                    , Html.li
                        [ classList
                            [ ( "active"
                              , isActive { link = Signup, page = page }
                              )
                            ]
                        ]
                        [ Html.a [ Attr.css [ Tw.py_1, Tw.px_4, Tw.text_xl, Tw.rounded, Tw.flex ], href "/signup" ] [ text "sign up" ] ]
                    ]
        ]


viewLoggedInHeader : { page : Page, token : Credentials.Token, openDropdown : Bool } -> Html Msg
viewLoggedInHeader { page, token, openDropdown } =
    Html.ul [ Attr.css [ Tw.flex, Tw.justify_between, Tw.gap_4, Tw.items_end ] ]
        [ case Jwt.decodeToken decodeTokenData <| fromTokenToString token of
            Ok resultTokenRecord ->
                Html.li
                    [ Attr.css [ Tw.cursor_pointer ] ]
                    [ Html.div [ Attr.css [ Tw.relative ] ]
                        [ if String.length resultTokenRecord.firstname > 0 then
                            Html.div
                                [ Attr.css [ Tw.flex, Tw.items_center ], onClick OpenDropdown ]
                                [ Html.div [ Attr.css [ Tw.w_10, Tw.h_10, Tw.overflow_hidden, Tw.rounded_full ] ]
                                    [ if String.isEmpty resultTokenRecord.profilepicurl then
                                        text ""

                                      else
                                        Html.img [ Attr.css [ Tw.w_10 ], src resultTokenRecord.profilepicurl ] []
                                    ]
                                , Html.span [ Attr.css [ Tw.py_1, Tw.px_4, Tw.text_xl ] ] [ text resultTokenRecord.firstname, Html.sup [ Attr.css [ Tw.ml_1 ] ] [ text "⌄" ] ]
                                ]

                          else
                            Html.div
                                [ onClick OpenDropdown ]
                                [ Html.span [ Attr.css [ Tw.py_1, Tw.px_4, Tw.text_xl ] ]
                                    [ text resultTokenRecord.email, Html.sup [ Attr.css [ Tw.ml_1 ] ] [ text "⌄" ] ]
                                , Html.div []
                                    [ if String.isEmpty resultTokenRecord.profilepicurl then
                                        text ""

                                      else
                                        Html.img [ src resultTokenRecord.profilepicurl, width 60 ] []
                                    ]
                                ]
                        , Html.ul
                            [ Attr.css [ Tw.flex, Tw.absolute, Tw.mt_3, Tw.flex_col, Tw.gap_1, Tw.overflow_hidden, Tw.transition_all, Tw.duration_500, Tw.bg_color Tw.white ]
                            , style "height"
                                (if openDropdown then
                                    "90px"

                                 else
                                    "0"
                                )
                            ]
                            [ Html.li
                                [ classList
                                    [ ( "active"
                                      , isActive { link = Profile resultTokenRecord.id, page = page }
                                      )
                                    ]
                                , onClick OpenDropdown
                                ]
                                [ Html.a [ Attr.css [ Tw.flex, Tw.py_1, Tw.px_4, Tw.rounded ], href <| "/profile/" ++ userIdToString resultTokenRecord.id ] [ text "My profile" ] ]
                            , Html.li [ onClick OpenDropdown ] [ Html.a [ Attr.css [ Tw.flex, Tw.py_1, Tw.px_4, Tw.rounded ] ] [ text "option2" ] ]
                            , Html.li [ onClick OpenDropdown ] [ Html.a [ Attr.css [ Tw.flex, Tw.py_1, Tw.px_4, Tw.rounded ] ] [ text "option3" ] ]
                            ]
                        ]
                    ]

            Err err ->
                Html.li [] [ text (Debug.toString err) ]
        , Html.li
            []
            [ Html.a [ Attr.css [ Tw.py_1, Tw.px_4, Tw.text_xl, Tw.rounded, Tw.flex ], href "/", onClick GetLogout ] [ text "logout" ] ]
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
                newPage =
                    urlToPage url model.session
            in
            initCurrentPage ( url, { model | page = newPage }, Cmd.none )

        GotLoginMsg loginMsg ->
            case model.page of
                LoginPage loginModel ->
                    let
                        ( loginModelFromLogin, loginMsgFromLogin ) =
                            Login.update loginMsg loginModel
                    in
                    ( { model | page = LoginPage loginModelFromLogin }, Cmd.map GotLoginMsg loginMsgFromLogin )

                _ ->
                    ( model, Cmd.none )

        GotProfileMsg profileMsg ->
            case model.page of
                ProfilePage profileModel ->
                    let
                        ( profileModelFromProfile, profileMsgFromProfile ) =
                            Profile.update profileMsg profileModel
                    in
                    ( { model | page = ProfilePage profileModelFromProfile }, Cmd.map GotProfileMsg profileMsgFromProfile )

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
                        ( verificationModelFromVerification, verificationMsgFromVerification ) =
                            Verification.update verificationMsg verificationModel
                    in
                    ( { model | page = VerificationPage verificationModelFromVerification }, Cmd.map GotVerificationMsg verificationMsgFromVerification )

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
            , case fromSessionToToken session of
                Just token ->
                    case Jwt.decodeToken decodeTokenData <| fromTokenToString token of
                        Ok resultTokenRecord ->
                            Nav.pushUrl model.key ("/profile/" ++ userIdToString resultTokenRecord.id)

                        Err _ ->
                            Nav.pushUrl model.key "/login"

                Nothing ->
                    Nav.pushUrl model.key "/login"
            )

        CheckSessionExpired ( session, maybeTime ) ->
            case ( maybeTime, fromSessionToToken session ) of
                ( Just time, Just token ) ->
                    let
                        tokenString =
                            fromTokenToString token
                    in
                    case Jwt.isExpired time tokenString of
                        Ok isExpired ->
                            if isExpired then
                                ( model, logout )

                            else
                                ( model, Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GetLogout ->
            ( model, logout )

        GotTime time ->
            ( { model | time = Just time }, Cmd.none )

        OpenDropdown ->
            ( { model | openDropdown = not model.openDropdown }, Cmd.none )


matchRoute : Parser (Route -> a) a
matchRoute =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map ForgotPassword (s "forgot-password")
        , Parser.map Login (s "login")
        , Parser.map Profile (s "profile" </> userIdParser)
        , Parser.map Signup (s "signup")
        , Parser.map Verification (s "verify-email" </> verifictionStringParser)
        , Parser.map ResetPassword (s "password-reset" </> passwordCodeStringParser)
        ]


urlToPage : Url -> Session -> Page
urlToPage url session =
    case Parser.parse matchRoute url of
        Just Login ->
            if fromSessionToToken session == Nothing then
                LoginPage (Tuple.first (Login.init ()))

            else
                NotFoundPage

        Just Signup ->
            if fromSessionToToken session == Nothing then
                SignupPage (Tuple.first (Signup.init ()))

            else
                NotFoundPage

        Just (Profile _) ->
            if fromSessionToToken session == Nothing then
                NotFoundPage

            else
                ProfilePage (Tuple.first (Profile.init session))

        Just (Verification _) ->
            if fromSessionToToken session == Nothing then
                NotFoundPage

            else
                VerificationPage (Tuple.first (Verification.init session url.path))

        Just (ResetPassword _) ->
            if fromSessionToToken session == Nothing then
                ResetPasswordPage (Tuple.first (ResetPassword.init url.path))

            else
                NotFoundPage

        Just ForgotPassword ->
            if fromSessionToToken session == Nothing then
                ForgotPasswordPage (Tuple.first (ForgotPassword.init ()))

            else
                NotFoundPage

        Just Home ->
            HomePage (Tuple.first (Home.init ()))

        Just NotFound ->
            NotFoundPage

        Nothing ->
            NotFoundPage


initCurrentPage : ( Url, Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( url, model, existingCmds ) =
    case model.page of
        NotFoundPage ->
            ( { model | page = NotFoundPage }, Cmd.none )

        LoginPage _ ->
            let
                ( pageModel, pageCmds ) =
                    Login.init ()

                -- Because Main doesn’t know anything about the page specific messages, it needs to map them to one of the data constructors from its own Msg type using the Cmd.map function
            in
            ( { model | page = LoginPage pageModel }, Cmd.map GotLoginMsg pageCmds )

        SignupPage _ ->
            let
                ( pageModel, pageCmds ) =
                    Signup.init ()
            in
            ( { model | page = SignupPage pageModel }, Cmd.map GotSignupMsg pageCmds )

        HomePage _ ->
            let
                ( pageModel, pageCmds ) =
                    Home.init ()
            in
            ( { model | page = HomePage pageModel }, Cmd.batch [ Cmd.map GotHomeMsg pageCmds, existingCmds ] )

        ForgotPasswordPage _ ->
            let
                ( pageModel, pageCmds ) =
                    ForgotPassword.init ()
            in
            ( { model | page = ForgotPasswordPage pageModel }, Cmd.batch [ Cmd.map GotFpMsg pageCmds, existingCmds ] )

        ResetPasswordPage _ ->
            let
                ( pageModel, pageCmds ) =
                    ResetPassword.init url.path
            in
            ( { model | page = ResetPasswordPage pageModel }, Cmd.map GotRpMsg pageCmds )

        VerificationPage _ ->
            let
                ( pageModel, pageCmds ) =
                    Verification.init model.session url.path
            in
            ( { model | page = VerificationPage pageModel }, Cmd.map GotVerificationMsg pageCmds )

        ProfilePage _ ->
            let
                ( pageModel, pageCmds ) =
                    Profile.init model.session
            in
            ( { model | page = ProfilePage pageModel }, Cmd.batch [ Cmd.map GotProfileMsg pageCmds, existingCmds ] )


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        session =
            decodeToSession key flags

        model =
            { page = urlToPage url session
            , key = key
            , session = session
            , openDropdown = False
            , time = Nothing
            }
    in
    initCurrentPage ( url, model, Task.perform GotTime Time.now )


subscriptions : Model -> Sub Msg
subscriptions model =
    subscriptionChanges GotSubscriptionChangeMsg model.key


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
