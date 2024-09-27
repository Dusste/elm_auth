module Components.SvgIcon exposing (..)

import Html
import Svg
import Svg.Attributes as HA
import Svg.Events


iconWarning : Html.Html msg
iconWarning =
    Svg.svg
        [ HA.viewBox "0 0 24 24"
        , HA.fill "none"
        , HA.width "20px"
        ]
        [ Svg.path
            [ HA.d "M12 17.0001H12.01M12 10.0001V14.0001M6.41209 21.0001H17.588C19.3696 21.0001 20.2604 21.0001 20.783 20.6254C21.2389 20.2985 21.5365 19.7951 21.6033 19.238C21.6798 18.5996 21.2505 17.819 20.3918 16.2579L14.8039 6.09805C13.8897 4.4359 13.4326 3.60482 12.8286 3.32987C12.3022 3.09024 11.6978 3.09024 11.1714 3.32987C10.5674 3.60482 10.1103 4.4359 9.19614 6.09805L3.6082 16.2579C2.74959 17.819 2.32028 18.5996 2.39677 19.238C2.46351 19.7951 2.76116 20.2985 3.21709 20.6254C3.7396 21.0001 4.63043 21.0001 6.41209 21.0001Z"
            , HA.stroke "white"
            , HA.strokeWidth "2"
            , HA.strokeLinecap "round"
            , HA.strokeLinejoin "round"
            ]
            []
        ]


iconArrowDown : Html.Html msg
iconArrowDown =
    Svg.svg
        [ HA.viewBox "0 -4.5 20 20" ]
        [ Svg.g
            [ HA.stroke "none", HA.strokeWidth "1", HA.fill "none", HA.fillRule "evenodd" ]
            [ Svg.g
                [ HA.transform "translate(-180.000000, -6684.000000)", HA.fill "#000000" ]
                [ Svg.g
                    [ HA.transform "translate(56.000000, 160.000000)" ]
                    [ Svg.path
                        [ HA.d "M144,6525.39 L142.594,6524 L133.987,6532.261 L133.069,6531.38 L133.074,6531.385 L125.427,6524.045 L124,6525.414 C126.113,6527.443 132.014,6533.107 133.987,6535 C135.453,6533.594 134.024,6534.965 144,6525.39" ]
                        []
                    ]
                ]
            ]
        ]



-- <svg width="800px" height="800px" viewBox="0 -4.5 20 20" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
--     <title>arrow_down [#339]</title>
--     <g id="Page-1" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd">
--         <g id="Dribbble-Light-Preview" transform="translate(-180.000000, -6684.000000)" fill="#000000">
--             <g id="icons" transform="translate(56.000000, 160.000000)">
--                 <path d="M144,6525.39 L142.594,6524 L133.987,6532.261 L133.069,6531.38 L133.074,6531.385 L125.427,6524.045 L124,6525.414 C126.113,6527.443 132.014,6533.107 133.987,6535 C135.453,6533.594 134.024,6534.965 144,6525.39" id="arrow_down-[#339]">
-- </path>
--             </g>
--         </g>
--     </g>
-- </svg>
-- <?xml version="1.0" encoding="utf-8"?><!-- Uploaded to: SVG Repo, www.svgrepo.com, Generator: SVG Repo Mixer Tools -->
-- <svg width="800px" height="800px" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg">
-- <path d="M12 17.0001H12.01M12 10.0001V14.0001M6.41209 21.0001H17.588C19.3696 21.0001 20.2604 21.0001 20.783 20.6254C21.2389 20.2985 21.5365 19.7951 21.6033 19.238C21.6798 18.5996 21.2505 17.819 20.3918 16.2579L14.8039 6.09805C13.8897 4.4359 13.4326 3.60482 12.8286 3.32987C12.3022 3.09024 11.6978 3.09024 11.1714 3.32987C10.5674 3.60482 10.1103 4.4359 9.19614 6.09805L3.6082 16.2579C2.74959 17.819 2.32028 18.5996 2.39677 19.238C2.46351 19.7951 2.76116 20.2985 3.21709 20.6254C3.7396 21.0001 4.63043 21.0001 6.41209 21.0001Z" stroke="#000000" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/>
-- </svg>
