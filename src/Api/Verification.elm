module Api.Verification exposing (apiCallAfterSomeTime, apiCallToVerify)

import Data.Credentials as Credentials
import Http
import Process
import Task


apiCallAfterSomeTime : Credentials.Session -> (Credentials.Session -> msg) -> Cmd msg
apiCallAfterSomeTime session toMsg =
    Process.sleep 5000
        |> Task.perform
            (\_ -> toMsg session)


apiCallToVerify : Credentials.Session -> (Result Http.Error Credentials.Token -> msg) -> Cmd msg
apiCallToVerify session toMsg =
    case Credentials.fromSessionToToken session of
        Just token ->
            Http.request
                { method = "PUT"
                , headers = [ Credentials.addHeader token ]
                , url = "/api/verify"
                , expect = Http.expectJson toMsg Credentials.tokenDecoder
                , body = Http.emptyBody
                , timeout = Nothing
                , tracker = Nothing
                }

        Nothing ->
            Cmd.none
