module Components.Misc exposing (loadingElement, sleepForAWhileThenCall)

import Data.Credentials as Credentials
import Html exposing (Html)
import Html.Attributes as HA
import Process
import Task


loadingElement : Html msg
loadingElement =
    Html.div
        [ HA.class "relative h-[20px] w-[20px] flex"
        ]
        [ Html.span
            [ HA.class "animate-ping absolute inline-flex h-full w-full rounded-full bg-sky-400 opacity-75"
            ]
            []
        , Html.span
            [ HA.class "relative inline-flex rounded-full h-[20px] w-[20px] bg-sky-500" ]
            []
        ]


sleepForAWhileThenCall : Credentials.Token -> (Credentials.Token -> msg) -> Cmd msg
sleepForAWhileThenCall token toMsg =
    Process.sleep 5000
        |> Task.perform
            (\_ -> toMsg token)
