module Chat exposing (..)

import Credentials exposing (Session, SocketMessageData, addUserToRoom, decodeSocketMessage, decodeTokenData, emitTyping, fromSessionToToken, fromTokenToString, sendMessageToSocket, userIdToString)
import Css
import Css.Global
import GlobalStyles as Gs
import Helpers exposing (buildErrorMessage)
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Event
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Jwt
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Time


type alias Model =
    { message : String
    , receivedMessages : List SocketMessageData
    , error : Maybe String
    , users : List User
    }


type CheckErrors
    = BadInput String
    | BadRequest String


type alias User =
    { firstname : String
    , email : String
    }


type Msg
    = StoreMessage String
    | MessageSubmit
    | MessageReceived SocketMessageData
    | FetchUsers (Result Http.Error (List User))
    | ChatMessages (Result Http.Error (List SocketMessageData))


unfoldMessageReceived : SocketMessageData -> Msg
unfoldMessageReceived socketData =
    MessageReceived socketData


initialModel : Model
initialModel =
    { message = ""
    , receivedMessages =
        []
    , error = Nothing
    , users = []
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoreMessage message ->
            ( { model | message = message }, emitTyping message )

        MessageSubmit ->
            ( { model | message = "" }, sendMessageToSocket model.message )

        MessageReceived message ->
            ( { model | receivedMessages = model.receivedMessages ++ [ message ] }, Cmd.none )

        FetchUsers (Ok users) ->
            ( { model | users = users }, Cmd.none )

        FetchUsers (Err err) ->
            ( { model | error = Just <| buildErrorMessage err }, Cmd.none )

        ChatMessages (Ok messages) ->
            ( { model | receivedMessages = model.receivedMessages ++ messages }, Cmd.none )

        ChatMessages (Err err) ->
            ( { model | error = Just <| buildErrorMessage err }, Cmd.none )


init : Session -> ( Model, Cmd Msg )
init session =
    case fromSessionToToken session of
        Just token ->
            case Jwt.decodeToken decodeTokenData <| fromTokenToString token of
                Ok resultTokenRecord ->
                    ( initialModel
                    , Cmd.batch
                        [ fetchUsers
                        , addUserToRoom
                            { userName =
                                takeNameOrEmail
                                    { firstname = resultTokenRecord.firstname
                                    , email = resultTokenRecord.email
                                    }
                            , userId = userIdToString resultTokenRecord.id
                            }
                        , fetchChatMessages
                        ]
                    )

                Err _ ->
                    ( initialModel, Cmd.none )

        Nothing ->
            ( initialModel, Cmd.none )


takeNameOrEmail : User -> String
takeNameOrEmail { firstname, email } =
    if String.isEmpty firstname then
        email

    else
        firstname


usersDecoder : Decoder (List User)
usersDecoder =
    Decode.list userDecoder


userDecoder : Decoder User
userDecoder =
    Decode.map2 User
        (Decode.field "firstname" string)
        (Decode.field "email" string)


fetchUsers : Cmd Msg
fetchUsers =
    Http.get
        { url = "/api/socket?roomId=123"
        , expect = Http.expectJson FetchUsers usersDecoder
        }


fetchChatMessages : Cmd Msg
fetchChatMessages =
    Http.get
        { url = "/api/messages"
        , expect = Http.expectJson ChatMessages (Decode.list decodeSocketMessage)
        }


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.css [ Tw.flex, Tw.flex_col ] ]
        [ Html.div [ Attr.css [ Tw.flex, Tw.gap_4, Tw.mx_2 ] ]
            [ Html.div [ Attr.css [ Tw.w_80, Tw.bg_color Tw.sky_200, Tw.p_4, Tw.rounded ] ]
                [ Html.h3 [ Attr.css [ Tw.m_0 ] ] [ text "Participants" ]
                , Html.div []
                    [ Html.ul [ Attr.css [ Tw.mt_4 ] ]
                        (model.users
                            |> List.map
                                (\user ->
                                    Html.li [ Attr.css [ Tw.mt_4 ] ] [ text <| takeNameOrEmail user ]
                                )
                        )
                    ]
                ]
            , Html.div [ Attr.css [ Tw.flex_grow ] ]
                [ Html.div [ Attr.class "customHeight" ] [ Html.ul [] (List.map viewMessage model.receivedMessages) ]
                , case model.error of
                    Just err ->
                        Html.div [] [ text err ]

                    Nothing ->
                        Html.div [ Attr.css [ Tw.py_4, Tw.flex, Tw.gap_4 ] ]
                            [ Html.textarea
                                [ Attr.css Gs.inputStyle
                                , Event.onInput StoreMessage
                                , Attr.value model.message
                                ]
                                []
                            , Html.button
                                [ Attr.type_ "button"
                                , Attr.css Gs.buttonStyle
                                , Event.onClick MessageSubmit
                                ]
                                [ text "Send" ]
                            ]
                ]
            ]
        ]


viewMessage : SocketMessageData -> Html Msg
viewMessage messageData =
    let
        hour =
            String.fromInt (Time.toHour Time.utc (Time.millisToPosix messageData.timestamp))

        minute =
            String.fromInt (Time.toMinute Time.utc (Time.millisToPosix messageData.timestamp))
    in
    Html.li [ Attr.css [ Tw.bg_color Tw.sky_200, Tw.p_2, Tw.rounded, Tw.mb_4 ] ]
        [ Html.div [ Attr.css [ Tw.flex, Tw.justify_between, Tw.items_center ] ]
            [ Html.div [ Attr.css [ Tw.flex, Tw.flex_col, Tw.gap_1 ] ]
                [ Html.div [ Attr.css [ Tw.text_color Tw.black ] ] [ text <| messageData.name ++ ":" ]
                , Html.div [] [ text <| messageData.data.message ]
                ]
            , Html.div [ Attr.css [ Tw.text_color Tw.gray_400 ] ] [ text <| hour ++ ":" ++ minute ]
            ]
        ]
