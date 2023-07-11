module GlobalStyles exposing (..)

import Css
import Tailwind.Breakpoints as Bp
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw


inputStyle : List Css.Style
inputStyle =
    [ Tw.block
    , Tw.w_full
    , Tw.form_input
    , Tw.rounded_md
    , Tw.border_0
    , Tw.py_1_dot_5
    , Tw.text_color Tw.gray_900
    , Tw.shadow_sm
    , Tw.ring_2
    , Tw.ring_inset
    , Tw.ring_color Tw.gray_900
    , Css.focus
        [ Tw.ring_2
        , Tw.ring_inset
        , Tw.ring_color Tw.sky_400
        ]
    ]


buttonStyle : List Css.Style
buttonStyle =
    [ Tw.bg_color Tw.sky_400
    , Tw.text_color Tw.white
    , Tw.py_1
    , Tw.px_4
    , Tw.text_xl
    , Tw.border
    , Tw.border_color Tw.sky_400
    , Tw.rounded
    , Tw.cursor_pointer
    , Tw.transition_all
    , Css.hover
        [ Tw.bg_color Tw.sky_900
        , Tw.border_color Tw.sky_900
        , Tw.border_color Tw.transparent
        ]
    ]
