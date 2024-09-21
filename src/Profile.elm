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
    , editMode : Bool
    , errors : Dict String (List String)
    }


type FormState
    = Initial
    | Loading
    | Error String


type UserState
    = NotVerified
    | Verified Credentials.Token
    | Intruder
    | SessionExpired


type Msg
    = StoreFirstName String
    | ProfileSubmit Credentials.Token
    | ProfileDone (Result Http.Error Credentials.Token)
    | FileRequest
    | FileRequestProceed File
    | FileRead (Result Http.Error String)
    | CancelEdit
    | EditMode


initialModel : Model
initialModel =
    { storeName = ""
    , profilePic = Nothing
    , formState = Initial
    , userState = NotVerified
    , editMode = False
    , errors = Dict.empty
    }


init : Credentials.Token -> ( Model, Cmd Msg )
init token =
    case Credentials.tokenToUserData token of
        Ok userDataFromToken ->
            ( { initialModel
                | storeName = userDataFromToken.firstname
                , userState =
                    if userDataFromToken.isverified then
                        Verified token

                    else
                        NotVerified
              }
            , Cmd.none
            )

        Err _ ->
            ( { initialModel | userState = Intruder }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case model.userState of
        Verified token ->
            Html.div
                [ -- HA.class [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.m_6, Tw.relative, Bp.sm [ Tw.m_20 ] ]
                  HA.class "flex flex-col self-center mt-32 relative w-[400px] gap-y-4"
                ]
                [ Html.h2
                    [-- HA.class [ Tw.text_3xl ]
                    ]
                    [ Html.text "Hello" ]
                , case model.formState of
                    Loading ->
                        Html.div
                            [ HA.class "absolute w-full h-full flex justify-center items-center bg-sky-50 bg-opacity-40"
                            ]
                            [ Components.Misc.loadingElement ]

                    Error error ->
                        Html.p
                            [ HA.class "text-red-500"
                            ]
                            [ Html.text error ]

                    Initial ->
                        Html.text ""
                , if model.editMode then
                    Html.form
                        [ HA.class "flex flex-col gap-y-4 w-full"
                        ]
                        [ Html.div
                            [ HA.class "flex flex-col gap-y-4"
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
                            [ HA.class "flex"
                            ]
                            [ Components.Element.button
                                |> Components.Element.withText "Choose file"
                                |> Components.Element.withMsg FileRequest
                                |> Components.Element.withDisabled False
                                |> Components.Element.withSecondaryStyle
                                |> Components.Element.toHtml
                            , Components.Element.notification
                                (Components.Element.Info "Upload an avatar (Size limit is 3mb)")
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
                            [ HA.class "mt-4 flex gap-x-4" ]
                            [ Components.Element.button
                                |> Components.Element.withText "Submit"
                                |> Components.Element.withMsg (ProfileSubmit token)
                                |> Components.Element.withDisabled False
                                |> Components.Element.withPrimaryStyle
                                |> Components.Element.toHtml
                            , Components.Element.button
                                |> Components.Element.withText "Cancel"
                                |> Components.Element.withMsg CancelEdit
                                |> Components.Element.withDisabled False
                                |> Components.Element.withSecondaryStyle
                                |> Components.Element.toHtml
                            ]
                        ]

                  else
                    Html.div
                        [ HA.class "mt-4" ]
                        [ Html.p
                            [ HA.class "mb-4" ]
                            [ Html.text "First Name:"
                            , Html.p
                                []
                                [ Html.text model.storeName ]
                            ]
                        , case Credentials.tokenToAvatar token of
                            Just imgSrc ->
                                Html.p
                                    [ HA.class "mb-4" ]
                                    [ Html.text "Your Avatar:"
                                    , Html.p
                                        []
                                        [ Html.img [ HA.src imgSrc ] [] ]
                                    ]

                            Nothing ->
                                Html.text ""
                        , Components.Element.button
                            |> Components.Element.withText "Edit"
                            |> Components.Element.withMsg EditMode
                            |> Components.Element.withDisabled False
                            |> Components.Element.withSecondaryStyle
                            |> Components.Element.toHtml
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

        EditMode ->
            ( { model | editMode = model.editMode |> not }, Cmd.none )

        CancelEdit ->
            ( { model | editMode = False }, Cmd.none )

        ProfileSubmit token ->
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
                    token
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
