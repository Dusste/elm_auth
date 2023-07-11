module Profile exposing (..)

import Credentials
    exposing
        ( Session
        , Token
        , addHeader
        , decodeTokenData
        , encodeImageString
        , encodeToken
        , fromSessionToToken
        , fromTokenToString
        , logout
        , storeSession
        , tokenDecoder
        )
import File exposing (File)
import File.Select as Select
import GlobalStyles as Gs
import Helpers exposing (buildErrorMessage, loadingElement)
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr exposing (src, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Json.Encode as Encode exposing (encode)
import Jwt
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Task


type alias Model =
    { storeName : String
    , profilePic : Maybe String
    , userState : UserState
    , formState : FormState
    }


type FormState
    = Initial
    | Loading
    | Error String


type UserState
    = NotVerified
    | Verified Session
    | Intruder
    | SessionExpired


type Msg
    = StoreFirstName String
    | ProfileSubmit Session
    | ProfileDone (Result Http.Error Token)
    | FileRequest
    | FileRequestProceed File
    | FileRead (Result Http.Error String)


profileSubmitDataEncoder : { name : String, profilePic : String } -> Encode.Value
profileSubmitDataEncoder { name, profilePic } =
    Encode.object
        [ ( "firstname", Encode.string name )
        , ( "imagefile", encodeImageString profilePic )
        ]


initialModel : Model
initialModel =
    { storeName = ""
    , profilePic = Nothing
    , formState = Initial
    , userState = NotVerified
    }


init : Session -> ( Model, Cmd Msg )
init session =
    case fromSessionToToken session of
        Just token ->
            case Jwt.decodeToken decodeTokenData <| fromTokenToString token of
                Ok userDataFromToken ->
                    ( { initialModel
                        | storeName = userDataFromToken.firstname
                        , userState =
                            if userDataFromToken.isverified then
                                Verified session

                            else
                                NotVerified
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { initialModel | userState = Intruder }
                    , Cmd.none
                    )

        Nothing ->
            ( { initialModel | userState = Intruder }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case model.userState of
        Verified session ->
            Html.div
                [ Attr.css [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_6, Tw.relative, Bp.sm [ Tw.m_20 ] ] ]
                [ Html.h2 [ Attr.css [ Tw.text_3xl ] ] [ text "Hello" ]
                , case model.formState of
                    Loading ->
                        Html.div [ Attr.css [ Tw.absolute, Tw.w_full, Tw.h_full, Tw.flex, Tw.justify_center, Tw.items_center, Tw.bg_color Tw.sky_50, Tw.bg_opacity_40 ] ] [ loadingElement ]

                    Error error ->
                        Html.p [ Attr.css [ Tw.text_color Tw.red_400 ] ] [ text error ]

                    Initial ->
                        text ""
                , Html.form [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_5, Tw.text_xl, Tw.w_full, Bp.md [ Tw.w_60 ] ] ]
                    [ Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_3 ] ]
                        [ text "First Name"
                        , Html.input
                            [ Attr.css Gs.inputStyle
                            , type_ "text"
                            , onInput StoreFirstName
                            , value model.storeName
                            ]
                            []
                        ]
                    , Html.div [ Attr.css [ Tw.flex, Tw.gap_3 ] ]
                        [ Html.div [ Attr.css [ Tw.flex, Tw.flex_col ] ]
                            [ Html.p [ Attr.css [ Tw.m_0 ] ] [ text "Upload an avatar" ]
                            , Html.p [ Attr.css [ Tw.m_0, Tw.text_sm, Tw.text_color Tw.gray_400 ] ] [ text "(Size limit is 3 mb)" ]
                            ]
                        , Html.label [ Attr.for "file", Attr.css <| Gs.buttonStyle ++ [ Tw.overflow_hidden ] ]
                            [ text "Choose file"
                            , Html.input
                                [ Attr.css [ Tw.w_1, Tw.h_1, Tw.overflow_hidden, Tw.opacity_0, Tw.absolute, Tw.z_0 ]
                                , Attr.id "file"
                                , type_ "file"
                                , onClick FileRequest
                                ]
                                []
                            ]
                        ]
                    , case model.profilePic of
                        Just imageString ->
                            Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_3 ] ]
                                [ text "Your avatar preview"
                                , Html.img
                                    [ Attr.css [ Tw.rounded ], src imageString ]
                                    []
                                ]

                        Nothing ->
                            text ""
                    , Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_3 ] ]
                        [ Html.button
                            [ Attr.css Gs.buttonStyle
                            , type_ "button"
                            , onClick (ProfileSubmit session)
                            ]
                            [ text "Submit" ]
                        ]
                    ]
                ]

        NotVerified ->
            Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_6, Bp.sm [ Tw.m_20 ] ] ]
                [ Html.h2 [] [ text "Please verify your email ! " ]
                , Html.p []
                    [ text "You can't access your profile until you verify your email" ]
                ]

        Intruder ->
            Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_6, Bp.sm [ Tw.m_20 ] ] ]
                [ Html.h2 [] [ text "Hmm seems you are not logged in" ]
                , Html.p []
                    [ text "Please create account or login" ]
                ]

        SessionExpired ->
            Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_6, Bp.sm [ Tw.m_20 ] ] ]
                [ Html.h2 [] [ text "Your session have expired" ]
                , Html.p []
                    [ text "Please login again" ]
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoreFirstName firstName ->
            ( { model | storeName = firstName }, Cmd.none )

        ProfileSubmit session ->
            let
                imageOrNot =
                    -- Allow user to send a form without new profile pic (but with new name only) - then BE won't proceed with changing it !
                    case model.profilePic of
                        Nothing ->
                            ""

                        Just imageUrl ->
                            imageUrl
            in
            if String.isEmpty model.storeName then
                ( { model | formState = Error "Name can't be empty" }, Cmd.none )

            else
                ( { model | formState = Loading }, submitProfile session { name = model.storeName, profilePic = imageOrNot } )

        ProfileDone (Ok token) ->
            let
                tokenValue =
                    encodeToken token
            in
            ( { model | formState = Initial }, storeSession <| Just <| encode 0 tokenValue )

        ProfileDone (Err error) ->
            ( { model | formState = Error <| buildErrorMessage error }
            , case error of
                Http.BadStatus statusCode ->
                    if statusCode == 401 then
                        logout

                    else
                        Cmd.none

                _ ->
                    Cmd.none
            )

        FileRequest ->
            ( model, Select.file [ "image/*" ] FileRequestProceed )

        FileRequestProceed file ->
            ( model, Task.attempt FileRead (File.toUrl file) )

        FileRead (Ok imageFileString) ->
            ( { model | profilePic = Just imageFileString }, Cmd.none )

        FileRead (Err error) ->
            ( { model | formState = Error <| buildErrorMessage error }, Cmd.none )


submitProfile : Session -> { profilePic : String, name : String } -> Cmd Msg
submitProfile session data =
    case fromSessionToToken session of
        Just token ->
            Http.request
                { method = "PUT"
                , headers = [ addHeader token ]
                , url = "/api/profile"
                , body = Http.jsonBody (profileSubmitDataEncoder data)
                , expect = Http.expectJson ProfileDone tokenDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

        Nothing ->
            Cmd.none
