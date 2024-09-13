module Profile exposing (..)

import Api.Profile
import Components.Element
import Components.Error
import Components.Misc
import Data.Credentials as Credentials
import Data.Ports as Ports
import Data.Validation as Validation
import Dict exposing (Dict)
import File exposing (File)
import File.Select as Select
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Encode
import Task


type alias Model =
    { storeName : String
    , profilePic : Maybe String
    , userState : UserState
    , formState : FormState
    , errors : Dict String (List String)
    }


type FormState
    = Initial
    | Loading
    | Error String


type UserState
    = NotVerified
    | Verified Credentials.Session
    | Intruder
    | SessionExpired


type Msg
    = StoreFirstName String
    | ProfileSubmit Credentials.Session
    | ProfileDone (Result Http.Error Credentials.Token)
    | FileRequest
    | FileRequestProceed File
    | FileRead (Result Http.Error String)


initialModel : Model
initialModel =
    { storeName = ""
    , profilePic = Nothing
    , formState = Initial
    , userState = NotVerified
    , errors = Dict.empty
    }


init : Credentials.Session -> ( Model, Cmd Msg )
init session =
    case Credentials.fromSessionToToken session of
        Just token ->
            case Credentials.tokenToUserData token of
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
                [-- HA.class [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_6, Tw.relative, Bp.sm [ Tw.m_20 ] ]
                ]
                [ Html.h2
                    [-- HA.class [ Tw.text_3xl ]
                    ]
                    [ Html.text "Hello" ]
                , case model.formState of
                    Loading ->
                        Html.div
                            [--  HA.class [ Tw.absolute, Tw.w_full, Tw.h_full, Tw.flex, Tw.justify_center, Tw.items_center, Tw.bg_color Tw.sky_50, Tw.bg_opacity_40 ]
                            ]
                            [ Components.Misc.loadingElement ]

                    Error error ->
                        Html.p
                            [-- HA.class [ Tw.text_color Tw.red_400 ]
                            ]
                            [ Html.text error ]

                    Initial ->
                        Html.text ""
                , Html.form
                    [-- HA.class [ Tw.flex, Tw.flex_col, Tw.gap_5, Tw.text_xl, Tw.w_full, Bp.md [ Tw.w_60 ] ]
                    ]
                    [ Html.div
                        [--  HA.class [ Tw.flex, Tw.flex_col, Tw.gap_3 ]
                        ]
                        [ Components.Element.inputField
                            { type_ = Components.Element.Text
                            , label = Just "First Name"
                            , value = model.storeName
                            , toMsg = StoreFirstName
                            , isDisabled = False
                            , error = Components.Error.byFieldName "name" model.errors
                            }
                        ]
                    , Html.div
                        [-- HA.class [ Tw.flex, Tw.gap_3 ]
                        ]
                        [ Html.div
                            [--  HA.class [ Tw.flex, Tw.flex_col ]
                            ]
                            [ Html.p
                                [-- HA.class [ Tw.m_0 ]
                                ]
                                [ Html.text "Upload an avatar" ]
                            , Html.p
                                [-- HA.class [ Tw.m_0, Tw.text_sm, Tw.text_color Tw.gray_400 ]
                                ]
                                [ Html.text "(Size limit is 3 mb)" ]
                            ]
                        , Html.label
                            [ HA.for "file"

                            -- , HA.class <| Gs.buttonStyle ++ [ Tw.overflow_hidden ]
                            ]
                            [ Html.text "Choose file"
                            , Html.input
                                [ -- HA.class [ Tw.w_1, Tw.h_1, Tw.overflow_hidden, Tw.opacity_0, Tw.absolute, Tw.z_0 ]
                                  HA.id "file"
                                , HA.type_ "file"
                                , HE.onClick FileRequest
                                ]
                                []
                            ]
                        ]
                    , case model.profilePic of
                        Just imageString ->
                            Html.div
                                [-- HA.class [ Tw.flex, Tw.flex_col, Tw.gap_3 ]
                                ]
                                [ Html.text "Your avatar preview"
                                , Html.img
                                    [ -- HA.class [ Tw.rounded ],
                                      HA.src imageString
                                    ]
                                    []
                                ]

                        Nothing ->
                            Html.text ""
                    , Html.div
                        [--  HA.class [ Tw.flex, Tw.flex_col, Tw.gap_3 ]
                        ]
                        [ Html.button
                            [ -- HA.class Gs.buttonStyle
                              HA.type_ "button"
                            , HE.onClick (ProfileSubmit session)
                            ]
                            [ Html.text "Submit" ]
                        ]
                    ]
                ]

        NotVerified ->
            Html.div
                [-- HA.class [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_6, Bp.sm [ Tw.m_20 ] ]
                ]
                [ Html.h2
                    []
                    [ Html.text "Please verify your email ! " ]
                , Html.p
                    []
                    [ Html.text "You can't access your profile until you verify your email" ]
                ]

        Intruder ->
            Html.div
                [--  HA.class [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_6, Bp.sm [ Tw.m_20 ] ]
                ]
                [ Html.h2
                    []
                    [ Html.text "Hmm seems you are not logged in" ]
                , Html.p
                    []
                    [ Html.text "Please create account or login" ]
                ]

        SessionExpired ->
            Html.div
                [-- HA.class [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_6, Bp.sm [ Tw.m_20 ] ]
                ]
                [ Html.h2
                    []
                    [ Html.text "Your session have expired" ]
                , Html.p
                    []
                    [ Html.text "Please login again" ]
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoreFirstName firstName ->
            let
                resetErrorsPerField : Dict String (List String)
                resetErrorsPerField =
                    Validation.resetErrorsPerField "name" model.errors
            in
            ( { model
                | storeName = firstName
                , errors = resetErrorsPerField
              }
            , Cmd.none
            )

        ProfileSubmit session ->
            let
                imageOrNot : String
                imageOrNot =
                    -- TODO Allow user to send a form without new profile pic (but with new name only) - then BE won't proceed with changing it !
                    case model.profilePic of
                        Nothing ->
                            ""

                        Just imageUrl ->
                            imageUrl

                validationConfig : Validation.Config
                validationConfig =
                    { validationRules =
                        [ { fieldName = "name"
                          , fieldRules = [ Validation.CheckEmptyName ]
                          , fieldValue = model.storeName
                          }
                        ]
                    , initialErrors = model.errors
                    }

                potentialErrors : Dict String (List String)
                potentialErrors =
                    Validation.checkErrors validationConfig
            in
            ( { model | errors = potentialErrors }
            , if Validation.anyActiveError potentialErrors then
                Cmd.none

              else
                Api.Profile.submitProfile
                    session
                    { name = model.storeName, profilePic = imageOrNot }
                    ProfileDone
            )

        ProfileDone (Ok token) ->
            let
                tokenValue =
                    Credentials.encodeToken token
            in
            ( { model | formState = Initial }
            , Ports.storeSession <| Just <| Json.Encode.encode 0 tokenValue
            )

        ProfileDone (Err error) ->
            ( { model | formState = Error <| Components.Error.buildErrorMessage error }
            , case error of
                Http.BadStatus statusCode ->
                    if statusCode == 401 then
                        Ports.logout

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
            ( { model | formState = Error <| Components.Error.buildErrorMessage error }, Cmd.none )
