module Data.Profile exposing (profileSubmitDataEncoder)

import Data.Credentials as Credentials
import Json.Encode as Encode


profileSubmitDataEncoder : { name : String, profilePic : String } -> Encode.Value
profileSubmitDataEncoder { name, profilePic } =
    Encode.object
        [ ( "firstname", Encode.string name )
        , ( "imagefile", Credentials.encodeImageString profilePic )
        ]
