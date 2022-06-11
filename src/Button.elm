module Button exposing (..)

import Color
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Material.Icons.Av
import Material.Icons.Content
import Material.Icons.File
import Material.Icons.Hardware
import Material.Icons.Navigation
import Svg exposing (Svg)
import Svg.Attributes


{-| Usage:

-- define problem-specific ButtonStyle and Button, then do:

buttonStyle : ButtonStyle -> Button.Style

buttonContent : Button -> Button.Content

button : ButtonStyle -> Button -> Maybe Msg -> Element Msg
button = Button.makeButton buttonStyle buttonContent Pressed

-}
type alias Style =
    { enabled : Element.Color
    , disabled : Element.Color
    , size : Int
    , borderRounding : Int
    , mouseOver : Maybe Element.Color
    }


type Content msg
    = Text String
    | Icon (Icon msg)


icon =
    { crossCircle = makeIcon Material.Icons.Navigation.cancel
    , down = makeIcon Material.Icons.Navigation.arrow_downward
    , up = makeIcon Material.Icons.Navigation.arrow_upward
    , inc = makeIcon Material.Icons.Hardware.keyboard_arrow_up
    , dec = makeIcon Material.Icons.Hardware.keyboard_arrow_down
    , save = makeIcon Material.Icons.Content.save
    , open = makeIcon Material.Icons.File.folder_open
    , edit = makeIcon Material.Icons.Content.create
    , tick = makeIcon Material.Icons.Navigation.check
    , cross = makeIcon Material.Icons.Content.clear
    , undo = makeIcon Material.Icons.Content.undo
    , redo = makeIcon Material.Icons.Content.redo
    , add = makeIcon Material.Icons.Content.add_circle
    , copy = makeIcon Material.Icons.Content.content_copy
    , play = makeIcon Material.Icons.Av.play_circle_filled
    , pause = makeIcon Material.Icons.Av.pause_circle_filled
    , stop = makeIcon Material.Icons.Av.stop
    }


{-| In your module, you need to define your ButtonStyles and Buttons. There's no harm having multiple distinct button types.

button : ButtonStyle -> Button -> Maybe Msg -> Element Msg
button = Button.makeButton buttonStyle buttonContent Pressed

data ButtonStyle = Normal | Large | Red | SmallGreen

defaultButtonStyle : Button.Style
defaultButtonStyle =
{ enabled = Colour.natural.paleBlue
, disabled = Colour.natural.lightGrey
, borderRounding = 10
, mouseOver = Just Colour.natural.paleGreen
, size = buttonSize.large
}

buttonStyle : ButtonStyle -> Button.Style
buttonStyle style = case style of
Normal -> defaultButtonStyle
Large -> { defaultButtonStyle | sixe = 40 }

data Msg = Initialise | TypedInBox | PressedUp | PressedDown | Went int

data Button = Up | Down | Go Int

buttonContent : Button -> Button.Content
buttonContent btn = case btn of
Up -> Button.icon.up
Down -> Button.icon.down
Go i -> Button.text <| String.fromInt

view model = Element.row [
button Normal Up PressedUp
button Normal Down PressedDown
button Large (Go 4) (Went 4)
]

-}
makeButton : (style -> Style) -> (button -> Content msg) -> button -> style -> Maybe msg -> Element msg
makeButton styleFunction contentFunction btn style enabledMsg =
    let
        { enabled, disabled, size, borderRounding, mouseOver } =
            styleFunction style

        colour =
            case enabledMsg of
                Just _ ->
                    enabled

                Nothing ->
                    disabled

        perhapsMouseOver =
            case ( mouseOver, enabledMsg ) of
                ( Just highlightColour, Just _ ) ->
                    \attrs ->
                        Element.mouseOver [ Element.Background.color highlightColour ] :: attrs

                _ ->
                    identity
    in
    Element.Input.button [ Element.centerY ]
        { onPress = enabledMsg
        , label =
            case contentFunction btn of
                Text string ->
                    Element.el
                        (perhapsMouseOver
                            [ Element.Border.rounded borderRounding
                            , Element.padding borderRounding
                            , Element.Background.color colour
                            , Element.Font.size size
                            ]
                        )
                        (Element.text string)

                Icon materialIcon ->
                    Element.el
                        (perhapsMouseOver
                            [ Element.Border.rounded borderRounding
                            ]
                        )
                        (materialIcon colour size)
        }


type alias MaterialIcon msg =
    Color.Color -> Int -> Svg msg


type alias Icon msg =
    Element.Color -> Int -> Element msg


iconAsElement : MaterialIcon msg -> Icon msg
iconAsElement ico elementColor size =
    let
        sizeString =
            String.fromInt size
    in
    Element.el [ Element.centerX, Element.centerY ] <|
        Element.html <|
            Svg.svg
                [ Svg.Attributes.width sizeString
                , Svg.Attributes.height sizeString
                , Svg.Attributes.viewBox <| String.join " " <| List.map String.fromInt [ 0, 0, size, size ]
                ]
                [ ico (elementColor |> Element.toRgb |> Color.fromRgba) size ]


makeIcon : MaterialIcon msg -> Content msg
makeIcon ico =
    Icon <| iconAsElement ico


text : String -> Content msg
text s =
    Text s
