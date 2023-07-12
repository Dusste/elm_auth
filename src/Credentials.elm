port module Credentials exposing
    ( ImageString
    , Session
    , Token
    , UserDataFromToken
    , UserId
    , VerificationString
    , addHeader
    , decodeToSession
    , decodeTokenData
    , emptyUserId
    , emptyVerificationString
    , encodeImageString
    , encodeToken
    , fromSessionToToken
    , fromTokenToString
    , logout
    , onSessionChange
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
import Json.Decode as Decode exposing (Decoder, Value, at, bool, map6, string)
import Json.Decode.Pipeline exposing (required)
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


subscriptionChanges : (Session -> msg) -> Nav.Key -> Sub msg
subscriptionChanges toMsg key =
    onSessionChange (\val -> toMsg (decodeToSession key val))


addHeader : Token -> Header
addHeader (Token tokenString) =
    header "authorization" ("Token " ++ tokenString)
