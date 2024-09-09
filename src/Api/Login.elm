module Api.Login exposing (submitLogin)

import Data.Credentials as Credentials
import Data.User as User
import Http


submitLogin : { email : String, password : String } -> (Result Http.Error Credentials.Token -> msg) -> Cmd msg
submitLogin creds toMsg =
    Http.post
        { url = "/api/login"
        , body = Http.jsonBody (User.credentialsEncoder creds)
        , expect = Http.expectJson toMsg Credentials.tokenDecoder
        }
