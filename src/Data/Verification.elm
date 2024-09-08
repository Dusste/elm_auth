module Data.Verification exposing
    ( VerificationString
    , verificationToString
    , verifictionStringParser
    , verifyStringDecoder
    )

import Json.Decode
import Url.Parser


type VerificationString
    = VerificationString String


verificationToString : VerificationString -> String
verificationToString (VerificationString verificationString) =
    -- TODO need some vaildation ?
    verificationString


verifyStringDecoder : Json.Decode.Decoder VerificationString
verifyStringDecoder =
    Json.Decode.map VerificationString Json.Decode.string


verifictionStringParser : Url.Parser.Parser (VerificationString -> a) a
verifictionStringParser =
    Url.Parser.custom "VERIFICATIONSTRING" <|
        \verificationString ->
            Maybe.map VerificationString (Just verificationString)
