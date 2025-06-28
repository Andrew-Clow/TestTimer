module Size exposing (..)

{-
    .d888888
   d8'    88
   88aaaaa88a 88d888b. 88d888b. .d8888b. .d8888b. 88d888b. .d8888b. 88d888b. .d8888b. .d8888b.
   88     88  88'  `88 88'  `88 88ooood8 88'  `88 88'  `88 88'  `88 88'  `88 88'  `"" 88ooood8
   88     88  88.  .88 88.  .88 88.  ... 88.  .88 88       88.  .88 88    88 88.  ... 88.  ...
   88     88  88Y888P' 88Y888P' `88888P' `88888P8 dP       `88888P8 dP    dP `88888P' `88888P'
              88       88
              dP       dP

   Appearance
-}

import Element exposing (Attribute, Element, rgb255)
import Element.Border as Border
import Element.Font as Font
import Element.Input
import Html.Events
import Json.Decode
import String exposing (fromInt)



{-----------------------------------------------------------------------------------------------------------------------

88                                    88               ,adba,           ad88                         88
88                                    88               8I  I8          d8"                           88
88                                    88               "8bdP'          88                            88
88           ,adPPYba,    ,adPPYba,   88   ,d8        ,d8"8b  88     MM88MMM  ,adPPYba,   ,adPPYba,  88
88          a8"     "8a  a8"     "8a  88 ,a8"       .dP'   Yb,8I       88    a8P_____88  a8P_____88  88
88          8b       d8  8b       d8  8888[         8P      888'       88    8PP"""""""  8PP"""""""  88
88          "8a,   ,a8"  "8a,   ,a8"  88`"Yba,      8b,   ,dP8b        88    "8b,        "8b,        88
88888888888  `"YbbdP"'    `"YbbdP"'   88   `Y8a     `Y8888P"  Yb       88     `"Ybbd8"'   `"Ybbd8"'  88


-----------------------------------------------------------------------------------------------------------------------}
-- Look & feel --


spacing =
    { small = Element.spacing 2
    , normal = Element.spacing 6
    , large = Element.spacing 30
    }


button =
    { large = 40
    , normal = 20
    , small = 8
    }


generousFont =
    { large = 40
    , normal = 20
    , small = 8
    }


padding =
    { small = Element.padding 6
    , normal = Element.padding 12
    , large = Element.padding 30
    }


fontSizeOld =
    { normal = Font.size 10
    , large = Font.size 16
    , huge = Font.size 24
    , enormous = Font.size 100
    }


borderRounded =
    { small = Border.rounded 3
    , normal = Border.rounded 6
    , large = Border.rounded 9
    }


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Json.Decode.field "key" Json.Decode.string
                |> Json.Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Json.Decode.succeed msg

                        else
                            Json.Decode.fail "Not the enter key"
                    )
            )
        )


onEnterOnEscape : msg -> msg -> Element.Attribute msg
onEnterOnEscape enterMsg escapeMsg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Json.Decode.field "key" Json.Decode.string
                |> Json.Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Json.Decode.succeed enterMsg

                        else if key == "Escape" then
                            Json.Decode.succeed escapeMsg

                        else
                            Json.Decode.fail "Not the enter key"
                    )
            )
        )


placeholderText : String -> Maybe (Element.Input.Placeholder msg)
placeholderText string =
    if string == "" then
        Nothing

    else
        Just <| Element.Input.placeholder [] <| Element.text string



{-

      type ChangeIntMsg
          = IncBy Int (Maybe Int)


      type HowToChangeNothing
          = TreatNothingAsZeroAndWrap
          | KeepTheNothingSoNoChange


      updateIncDecWithMax : Int -> HowToChangeNothing -> ChangeIntMsg -> Maybe Int
      updateIncDecWithMax max howTo incOrDec =
          case incOrDec of
              IncBy inc Nothing ->
                  case howTo of
                      TreatNothingAsZeroAndWrap ->
                          Just (modBy max inc)

                      KeepTheNothingSoNoChange ->
                          Nothing

              IncBy inc (Just val) ->
                  Just <| modBy max <| val + inc


      incDec : msg -> msg -> Element msg
      incDec i d =
          Element.column []
              [ click icons.incSmall i
              , click icons.decSmall d
              ]


      editInt : Int -> HowToChangeNothing -> Maybe Int -> Element (Maybe Int)
      editInt max howTo current =
          Element.row [ Element.width Element.fill ]
              [ Element.map (updateIncDecWithMax max howTo) <| incDec (IncBy 10 current) (IncBy -10 current)
              , inputPositiveIntegerBelow max [ Element.width Element.fill ] current
              , Element.map (updateIncDecWithMax max howTo) <| incDec (IncBy 1 current) (IncBy -1 current)
              ]


   {-


      88
      88
      88
      88   ,adPPYba,   ,adPPYba,   8b,dPPYba,   ,adPPYba,
      88  a8"     ""  a8"     "8a  88P'   `"8a  I8[    ""
      88  8b          8b       d8  88       88   `"Y8ba,
      88  "8a,   ,aa  "8a,   ,a8"  88       88  aa    ]8I
      88   `"Ybbd8"'   `"YbbdP"'   88       88  `"YbbdP"'


      Icons

   -}


   icons =
       { crossCircle = iconElement Material.Icons.Navigation.cancel iconSize.normal Color.black
       , down = iconElement Material.Icons.Navigation.arrow_downward iconSize.normal Color.black
       , up = iconElement Material.Icons.Navigation.arrow_upward iconSize.normal Color.black
       , save = iconElement Material.Icons.Content.save iconSize.normal Color.black
       , open = iconElement Material.Icons.File.folder_open iconSize.normal Color.black
       , edit = iconElement Material.Icons.Content.create iconSize.normal Color.black
       , tick = iconElement Material.Icons.Navigation.check iconSize.normal Color.black
       , cross = iconElement Material.Icons.Content.clear iconSize.normal Color.black
       , addLarge = iconElement Material.Icons.Content.add_circle iconSize.large Color.blue
       , undoLargeBlue = iconElement Material.Icons.Content.undo iconSize.large Color.blue
       , undoLargeGrey = iconElement Material.Icons.Content.undo iconSize.large Color.grey
       , redoLargeBlue = iconElement Material.Icons.Content.redo iconSize.large Color.blue
       , redoLargeGrey = iconElement Material.Icons.Content.redo iconSize.large Color.grey
       , addNormal = iconElement Material.Icons.Content.add_circle iconSize.normal Color.black
       , copyNormal = iconElement Material.Icons.Content.content_copy iconSize.normal Color.black
       , incSmall = iconElement Material.Icons.Hardware.keyboard_arrow_up iconSize.normal Color.black
       , decSmall = iconElement Material.Icons.Hardware.keyboard_arrow_down iconSize.normal Color.black
       , playGreen = iconElement Material.Icons.Av.play_circle_filled iconSize.normal Color.green
       , pauseGrey = iconElement Material.Icons.Av.pause_circle_filled iconSize.normal Color.grey
       , stopRed = iconElement Material.Icons.Av.stop iconSize.normal Color.darkRed
       , playGreenLarge = iconElement Material.Icons.Av.play_circle_filled iconSize.large Color.green
       , pauseGreyLarge = iconElement Material.Icons.Av.pause_circle_filled iconSize.large Color.grey
       , stopRedLarge = iconElement Material.Icons.Av.stop iconSize.large Color.darkRed
       }


   mouseOverColor : Element.Color -> Element msg -> Element msg
   mouseOverColor color thing =
       Element.el [ Element.mouseOver [ Background.color color ], Border.rounded 10 ] thing


   clickHoverColour : Element.Color -> Element msg -> msg -> Element msg
   clickHoverColour colour thing msg =
       mouseOverColor colour <|
           el [ onClick msg ] thing


   click : Element msg -> msg -> Element msg
   click icon msg =
       el [ onClick msg ] icon


   crossRHS : msg -> Element msg
   crossRHS msg =
       el
           [ onClick msg
           , alignRight
           ]
           icons.crossCircle


   iconSize =
       { normal = 20
       , large = 40
       , small = 8
       }


   iconElement : (Color.Color -> Int -> Svg msg) -> Int -> Color.Color -> Element msg
   iconElement icon size colorColor =
       let
           sizeString =
               fromInt size
       in
       el [ centerX, centerY ] <|
           Element.html <|
               Svg.svg
                   [ Svg.Attributes.width sizeString
                   , Svg.Attributes.height sizeString
                   , Svg.Attributes.viewBox <| String.join " " <| List.map fromInt [ 0, 0, size, size ]
                   ]
                   [ icon colorColor size ]


   type alias AutoButtonColors msg =
       { normalColor : Attribute msg
       , focusedColor : Element.Decoration
       , disabledColor : Attribute msg
       }


   autoButtonColors =
       { blue =
           { normalColor = Colour.buttonColor.blue
           , focusedColor = Colour.buttonColor.greenerBlue
           , disabledColor = Colour.buttonColor.disabled
           }
       }


   autoButton : { autoButtonColors : AutoButtonColors msg, label : Element msg, onPress : Maybe msg } -> Element msg
   autoButton info =
       let
           shape =
               [ borderRounded.normal
               , padding.normal
               ]

           attrs =
               case info.onPress of
                   Just a ->
                       shape ++ [ info.autoButtonColors.normalColor, Element.focused [ info.autoButtonColors.focusedColor ], Element.mouseOver [ info.autoButtonColors.focusedColor ] ]

                   --shape ++ [ info.autoButtonColors.normalColor, Element.focused [ Element.alpha 0.8, info.autoButtonColors.focusedColor ], Element.mouseOver [ Element.alpha 0.8, info.autoButtonColors.focusedColor ] ]
                   Nothing ->
                       shape ++ [ info.autoButtonColors.disabledColor ]
       in
       Element.Input.button attrs { onPress = info.onPress, label = info.label }


   type alias ButtonData msg =
       { color : Element.Color, label : String, upOrDown : UpOrDown, msg : msg }


   buttonFromList : Attribute msg -> msg -> List (Element msg) -> Bool -> Element msg
   buttonFromList colorAttribute msg insides disabled =
       if not disabled then
           row [ onClick msg, colorAttribute, borderRounded.normal, padding.normal, spacing.normal ] insides

       else
           row [ Colour.buttonColor.disabled, borderRounded.normal, padding.normal, spacing.normal ] insides


   tapButton : ButtonData msg -> Element msg
   tapButton { color, label, upOrDown, msg } =
       let
           ( finalColor, finalFontColor ) =
               case upOrDown of
                   Up ->
                       ( color, rgb 0 0 0 )

                   Down ->
                       ( rgb 1 1 1, rgb 0 0 0 )
       in
       paragraph
           [ Background.color finalColor
           , onClick msg
           , borderRounded.normal
           , padding.normal
           , Font.color finalFontColor
           ]
           [ text label ]

   errorPanel : List String -> Element String
   errorPanel errors =
       row [ width fill ]
           [ column [ width fill, spacing.normal ] <| List.map errorItem errors ]


   errorItem : String -> Element String
   errorItem e =
       row
           [ Colour.backgroundColor.errorLight
           , width fill
           , padding.normal
           ]
           [ text e
           , crossRHS e
           ]




-}
