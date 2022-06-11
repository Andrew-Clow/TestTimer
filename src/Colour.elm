module Colour exposing (..)

import Color
import Element exposing (rgb)
import Element.Background
import Element.Font


fade : Element.Color -> Element.Color
fade =
    withHsla <|
        \{ hue, saturation, lightness, alpha } ->
            { hue = hue
            , saturation = saturation
            , lightness =
                if lightness > 0.8 then
                    1

                else
                    lightness + 0.2
            , alpha = alpha
            }


natural =
    { black = Element.rgb255 2 7 14
    , darkGrey = Element.rgb255 157 155 156
    , midGrey = Element.rgb255 217 218 222
    , lightGrey = Element.rgb255 226 229 234
    , veryLightGrey = Element.rgb255 237 242 246
    , white = Element.rgb255 246 251 254
    , pink = Element.rgb255 252 220 244
    , red = Element.rgb255 217 34 44
    , maroon = Element.rgb 0.6 0 0.2
    , brown = Element.rgb255 127 71 40
    , mauve = Element.rgba255 203 152 167 1
    , orange = Element.rgb255 248 139 38
    , gold = Element.rgb255 241 202 4
    , yellow = Element.rgb255 250 237 78
    , lime = Element.rgb255 142 191 1
    , green = Element.rgb255 118 212 143
    , paleGreen = Element.rgb255 209 237 191
    , appleWhite = Element.rgb 0.85 1 0.85
    , cyan = Element.rgb255 137 242 228
    , teal = Element.rgb255 77 205 185
    , blue = Element.rgb255 72 144 226
    , paleBlue = Element.rgb255 177 209 250
    , lilac = Element.rgb255 161 149 219
    , purple = Element.rgb255 110 85 115
    }


ultraPale =
    { red = hsl 0 1 0.95
    , orange = hsl 45 1 0.9
    , yellow = hsl 60 1 0.9
    , green = hsl 90 1 0.9
    , blue = hsl 210 1 0.95
    , purple = hsl 270 1 0.95
    , grey = rgb 0.95 0.95 0.95
    }


pale =
    { red = hsl 0 1 0.9
    , orange = hsl 45 1 0.85
    , yellow = hsl 60 1 0.8
    , green = hsl 90 1 0.85
    , blue = hsl 210 1 0.9
    , purple = hsl 270 1 0.85
    , grey = rgb 0.9 0.9 0.9
    }


bright =
    { red = hsl 0 1 0.6
    , orange = hsl 40 1 0.5
    , yellow = hsl 60 1 0.5
    , green = hsl 90 1 0.5
    , blue = hsl 240 1 0.6
    , purple = hsl 270 1 0.6
    }


deep =
    { red = hsl 0 1 0.35
    , orange = hsl 35 1 0.4
    , yellow = hsl 60 0.8 0.5
    , green = hsl 90 1 0.25
    , blue = hsl 240 1 0.35
    , purple = hsl 270 0.9 0.4
    , grey = rgb 0.4 0.4 0.4
    }


appPalette =
    { blue = rgb 0.3255 0.3255 0.9176
    , greenerBlue = rgb 0.3255 0.6431 0.9176
    , disabled = rgb 0.9 0.9 0.9
    , errorLight = rgb 1 0 0
    , errorDark = rgb 0.5 0 0
    , green = rgb 0.1 0.9 0.3
    , default = rgb 0.7804 0.85 0.7804
    , shadow = rgb 0.4 0.4 0.4
    }


type alias HSLA =
    { hue : Float
    , saturation : Float
    , lightness : Float
    , alpha : Float
    }


hsl : Int -> Float -> Float -> Element.Color
hsl h s l =
    Color.hsl (toFloat h / 360) s l
        |> Color.toRgba
        |> (\rgba -> Element.rgb rgba.red rgba.green rgba.blue)


withHsla : (HSLA -> HSLA) -> Element.Color -> Element.Color
withHsla f c =
    c
        |> Element.toRgb
        |> Color.fromRgba
        |> Color.toHsla
        |> f
        |> Color.fromHsla
        |> Color.toRgba
        |> (\{ red, green, blue } -> Element.rgb red green blue)


backgroundColor =
    { errorLight = Element.Background.color appPalette.errorLight
    , errorDark = Element.Background.color appPalette.errorDark
    }


buttonColor =
    { blue = Element.Background.color appPalette.blue
    , greenerBlue = Element.Background.color appPalette.greenerBlue
    , disabled = Element.Background.color appPalette.disabled
    , green = Element.Background.color appPalette.green
    }


fontColor =
    { good = Element.Font.color (rgb 0 0.6 0)
    , bad = Element.Font.color (rgb 1 0.6 0)
    , grey = Element.Font.color (rgb 0.7 0.7 0.7)
    }


cast : Element.Color -> Color.Color
cast =
    Element.toRgb >> Color.fromRgba


{-| Helper function for using hex notation.

     hex 0xCCFF99 == rgb255 0xCC 0xFF 0x99

    If The number is too big, we just go white, if it's negative, we just go black.
    This is to allow you to hard-code colours into your source file without unwrapping a Maybe with a default colour.

-}
hex : Int -> Element.Color
hex anyNumber =
    let
        number =
            clamp 0 0x00FFFFFF anyNumber

        b =
            modBy 0x0100 number

        rg =
            number // 0x0100

        g =
            modBy 0x0100 rg

        r =
            rg // 0x0100
    in
    Element.rgb255 r g b


labelledHex : Int -> String -> Element.Color
labelledHex hexnum _ =
    hex hexnum
