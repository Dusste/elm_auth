module Components.Misc exposing (loadingElement, sleepForAWhileThenCall)

import Data.Credentials as Credentials
import Html exposing (Html)
import Process
import Task


loadingElement : Html msg
loadingElement =
    Html.div
        [--  HA.class [ Tw.relative, Tw.h_5, Tw.w_5, Tw.flex ]
        ]
        [ Html.span
            [-- HA.class [ Tw.animate_ping, Tw.absolute, Tw.inline_flex, Tw.h_full, Tw.w_full, Tw.rounded_full, Tw.bg_color Tw.sky_400, Tw.opacity_75 ]
            ]
            []
        , Html.span
            [-- HA.class [ Tw.relative, Tw.inline_flex, Tw.rounded_full, Tw.h_5, Tw.w_5, Tw.bg_color Tw.sky_500 ]
            ]
            []
        ]


sleepForAWhileThenCall : Credentials.Session -> (Credentials.Session -> msg) -> Cmd msg
sleepForAWhileThenCall session toMsg =
    Process.sleep 5000
        |> Task.perform
            (\_ -> toMsg session)
