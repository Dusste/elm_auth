port module Credentials exposing
    ( ImageString
    , Session
    , SocketMessageData
    , Token
    , UserDataFromToken
    , UserId
    , VerificationString
    , addHeader
    , addUserToRoom
    , decodeSocketMessage
    , decodeToSession
    , decodeTokenData
    , emitTyping
    , emptyUserId
    , emptyVerificationString
    , encodeImageString
    , encodeToken
    , fromSessionToToken
    , fromTokenToString
    , guest
    , initiateSocketChannel
    , logout
    , onSessionChange
    , sendMessageToSocket
    , socketMessageChanges
    , storeSession
    , subscriptionChanges
    , tokenDecoder
    , userIdParser
    , userIdToString
    , verificationToString
    , verifictionStringParser
    )

import Browser.Navigation as Nav
import Http exposing (Header, header)
import Json.Decode as Decode exposing (Decoder, Value, at, bool, int, map6, string)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Json.Encode as Encode exposing (Value)
import Url.Parser exposing (Parser, custom)


type Token
    = Token String


type Session
    = LoggedIn Token
    | Guest


type VerificationString
    = VerificationString String


type UserId
    = UserId String


type alias SocketMessageData =
    { name : String
    , id : String
    , clientId : String
    , connectionId : String
    , timestamp : Int
    , data : DataMessage
    }


type alias DataMessage =
    { message : String
    }



--    t{ name = Debug.toString err
--     , id = ""
--     , clientId = ""
--     , connectionId = ""
--     , timestamp = 0
--     , data =
--         { message = ""
--         }
--     }
-- {
--     name: 'hello-world-message',
--     id: 'W46cHmYS-f:5:0',
--     clientId: '123',
--     connectionId: 'W46cHmYS-f',
--     encoding: null,
--     data: {
--         message: '{msgType: hello-there-guys, msgText: Hello from Elm Side !}',
--     },
-- };


userIdToString : UserId -> String
userIdToString (UserId id) =
    -- TODO need some vaildation ?
    id


verificationToString : VerificationString -> String
verificationToString (VerificationString verificationString) =
    -- TODO need some vaildation ?
    verificationString


emptyUserId : UserId
emptyUserId =
    UserId ""


emptyVerificationString : VerificationString
emptyVerificationString =
    VerificationString ""


guest : Session
guest =
    Guest


userIdParser : Parser (UserId -> a) a
userIdParser =
    custom "USERID" <|
        \userId ->
            Maybe.map UserId (Just userId)


verifictionStringParser : Parser (VerificationString -> a) a
verifictionStringParser =
    custom "VERIFICATIONSTRING" <|
        \verificationString ->
            Maybe.map VerificationString (Just verificationString)


encodeUserId : UserId -> Encode.Value
encodeUserId (UserId id) =
    -- TODO need some vaildation ?
    Encode.string id


encodeImageString : ImageString -> Encode.Value
encodeImageString imageString =
    Encode.string imageString


idDecoder : Decoder UserId
idDecoder =
    Decode.map UserId string


verifyStringDecoder : Decoder VerificationString
verifyStringDecoder =
    Decode.map VerificationString string


type alias ImageString =
    String


type alias UserDataFromToken =
    { id : UserId
    , isverified : Bool
    , email : String
    , firstname : String
    , verificationstring : VerificationString
    , profilepicurl : ImageString
    }


fromSessionToToken : Session -> Maybe Token
fromSessionToToken session =
    case session of
        LoggedIn token ->
            Just token

        Guest ->
            Nothing



-- skontaj kako da validirsas da je string token
-- fromStringToToken : String -> Token
-- fromStringToToken string =
--     Token string


fromTokenToString : Token -> String
fromTokenToString (Token string) =
    string


tokenDecoder : Decoder Token
tokenDecoder =
    Decode.succeed Token
        |> required "token" string


encodeToken : Token -> Value
encodeToken (Token token) =
    Encode.object
        [ ( "token", Encode.string token ) ]


port storeSession : Maybe String -> Cmd msg


port onSessionChange : (Value -> msg) -> Sub msg


port listenSocketMessage : (Value -> msg) -> Sub msg


port initiateSocketChannel : String -> Cmd msg


port sendMessageToSocket : String -> Cmd msg


port emitTyping : String -> Cmd msg


port addUserToRoom : { userName : String, userId : String } -> Cmd msg


logout : Cmd msg
logout =
    storeSession Nothing


decodeTokenData : Decoder UserDataFromToken
decodeTokenData =
    map6 UserDataFromToken
        (at [ "id" ] idDecoder)
        (at [ "isverified" ] bool)
        (at [ "email" ] string)
        (at [ "firstname" ] string)
        (at [ "verificationstring" ] verifyStringDecoder)
        (at [ "profilepicurl" ] string)



{-
   You can run a decoder by using Json.Decode.decodeValue.
   Then you’ll get a Result Error UserDataFromToken.
   You can get rid of Result by using a case of and handling both the Ok validData and Err error cases.

-}


decodeToSession : Nav.Key -> Value -> Session
decodeToSession key value =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    case
        Decode.decodeValue Decode.string value
            |> Result.andThen (Decode.decodeString tokenDecoder)
            |> Result.toMaybe
    of
        Just token ->
            LoggedIn token

        Nothing ->
            Guest


decodeSocketMessage : Decoder SocketMessageData
decodeSocketMessage =
    Decode.succeed SocketMessageData
        |> required "name" string
        |> required "id" string
        |> required "clientId" string
        |> required "connectionId" string
        |> required "timestamp" int
        |> required "data" decodeMessage


decodeMessage : Decoder DataMessage
decodeMessage =
    Decode.succeed DataMessage
        |> required "message" string



-- { name = String
-- , id = String
-- , clientId = String
-- , connectionId = String
-- , encoding = String
-- , data =
--     { message = String
--     }
-- }


decodeToSocket : Nav.Key -> Value -> SocketMessageData
decodeToSocket key value =
    let
        result =
            Decode.decodeValue
                decodeSocketMessage
                value
    in
    case result of
        Ok obj ->
            obj

        Err err ->
            { name = Debug.toString err
            , id = ""
            , clientId = ""
            , connectionId = ""
            , timestamp = 0
            , data =
                { message = ""
                }
            }



-- decodeTokenData : Decoder UserDataFromToken
-- decodeTokenData =
--     map6 UserDataFromToken
--         (at [ "id" ] idDecoder)
--         (at [ "isverified" ] bool)
--         (at [ "email" ] string)
--         (at [ "firstname" ] string)
--         (at [ "verificationstring" ] verifyStringDecoder)
--         (at [ "profilepicurl" ] imageStringDecoder)


subscriptionChanges : (Session -> msg) -> Nav.Key -> Sub msg
subscriptionChanges toMsg key =
    onSessionChange (\val -> toMsg (decodeToSession key val))


socketMessageChanges : (SocketMessageData -> msg) -> Nav.Key -> Sub msg
socketMessageChanges toMsg key =
    listenSocketMessage (\val -> toMsg (decodeToSocket key val))


addHeader : Token -> Header
addHeader (Token tokenString) =
    header "authorization" ("Token " ++ tokenString)
