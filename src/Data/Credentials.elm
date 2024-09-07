module Data.Credentials exposing
    ( ImageString
    , ResetCodeParam
    , Session
    , Token
    , UserDataFromToken
    , UserId
    , addHeader
    , decodeToSession
    , decodeTokenData
    , encodeImageString
    , encodeToken
    , fromSessionToToken
    , fromTokenToString
    , passwordCodeStringParser
    , resetCodeParamToString
    , subscriptionChanges
    , tokenDecoder
    , userIdParser
    , userIdToString
    )

import Browser.Navigation as Nav
import Data.Ports as Ports
import Data.Verification as Verification
import Http
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Url.Parser


type Token
    = Token String


type Session
    = LoggedIn Token
    | Guest


type ResetCodeParam
    = ResetCodeParam String


type UserId
    = UserId String


resetCodeParamToString : ResetCodeParam -> String
resetCodeParamToString (ResetCodeParam str) =
    str


userIdToString : UserId -> String
userIdToString (UserId id) =
    -- TODO need some vaildation ?
    id


encodeImageString : ImageString -> Json.Encode.Value
encodeImageString imageString =
    Json.Encode.string imageString


idDecoder : Json.Decode.Decoder UserId
idDecoder =
    Json.Decode.map UserId Json.Decode.string


type alias ImageString =
    String


type alias UserDataFromToken =
    { id : UserId
    , isverified : Bool
    , email : String
    , firstname : String
    , verificationstring : Verification.VerificationString
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


tokenDecoder : Json.Decode.Decoder Token
tokenDecoder =
    Json.Decode.succeed Token
        |> Json.Decode.Pipeline.required "token" Json.Decode.string


encodeToken : Token -> Json.Encode.Value
encodeToken (Token token) =
    Json.Encode.object
        [ ( "token", Json.Encode.string token ) ]


decodeTokenData : Json.Decode.Decoder UserDataFromToken
decodeTokenData =
    Json.Decode.map6 UserDataFromToken
        (Json.Decode.at [ "id" ] idDecoder)
        (Json.Decode.at [ "isverified" ] Json.Decode.bool)
        (Json.Decode.at [ "email" ] Json.Decode.string)
        (Json.Decode.at [ "firstname" ] Json.Decode.string)
        (Json.Decode.at [ "verificationstring" ] Verification.verifyStringDecoder)
        (Json.Decode.at [ "profilepicurl" ] Json.Decode.string)



{-
   You can run a decoder by using Json.Decode.decodeValue.
   Then you’ll get a Result Error UserDataFromToken.
   You can get rid of Result by using a case of and handling both the Ok validData and Err error cases.

-}


decodeToSession : Nav.Key -> Json.Encode.Value -> Session
decodeToSession key value =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    case
        Json.Decode.decodeValue Json.Decode.string value
            |> Result.andThen (Json.Decode.decodeString tokenDecoder)
            |> Result.toMaybe
    of
        Just token ->
            LoggedIn token

        Nothing ->
            Guest


subscriptionChanges : (Session -> msg) -> Nav.Key -> Sub msg
subscriptionChanges toMsg key =
    Ports.onSessionChange (\val -> toMsg (decodeToSession key val))


addHeader : Token -> Http.Header
addHeader (Token tokenString) =
    Http.header "authorization" ("Token " ++ tokenString)


userIdParser : Url.Parser.Parser (UserId -> a) a
userIdParser =
    Url.Parser.custom "USERID" <|
        \userId ->
            Maybe.map UserId (Just userId)


passwordCodeStringParser : Url.Parser.Parser (ResetCodeParam -> a) a
passwordCodeStringParser =
    Url.Parser.custom "RESETCODEPARAM" <|
        \resetPassword ->
            Maybe.map ResetCodeParam (Just resetPassword)
