-------------------------------------------------
--                                             --
--   Credit and thanks to GaiaGonen            --
--   https://github.com/GaiaGonen/elm-clock/   --
--                                             --
-------------------------------------------------


module AnalogClock exposing (view)

import Element
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time



-- MODEL


type alias HasTime r =
    { r
        | timeZone : Time.Zone
        , currentTime : Time.Posix
    }



--VIEW


view : HasTime r -> Element.Element msg
view model =
    Element.html <|
        let
            hour =
                Time.toHour model.timeZone model.currentTime

            minute =
                Time.toMinute model.timeZone model.currentTime

            second =
                Time.toSecond model.timeZone model.currentTime
        in
        div []
            [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "styles.css" ] []
            , svg [ Svg.Attributes.width "600", Svg.Attributes.height "600" ]
                [ circle
                    [ cx "300"
                    , cy "300"
                    , r "220"
                    , stroke "rgb(85, 55, 6)"
                    , strokeWidth "10px"
                    , fill "rgb(255,255,255)"
                    ]
                    []
                , circle [ cx "300", cy "300", r "10", fill "rgb(85, 55, 6)" ] []
                , g [] <| List.map drawNumber <| List.range 1 12
                , drawHands second minute hour
                ]
            ]


type alias Point =
    { x : Int
    , y : Int
    }


drawHands : Int -> Int -> Int -> Svg msg
drawHands second minute hour =
    let
        center =
            Point 300 300

        secondsHand =
            findPointOnCircle 60 second 190 center

        secondsHandEnd =
            findPointOnCircle 60 second -30 center

        minutesHand =
            findPointOnCircle (60 * 60) (minute * 60 + second) 140 center

        minutesHandMiddleLeft =
            findPointOnCircle (60 * 60) (minute * 60 + second - 30) 40 center

        minutesHandMiddleRight =
            findPointOnCircle (60 * 60) (minute * 60 + second + 30) 40 center

        hoursHand =
            findPointOnCircle (12 * 60) (hour * 60 + minute) 80 center

        hoursHandMiddleLeft =
            findPointOnCircle (12 * 60) (hour * 60 + minute - 20) 28 center

        hoursHandMiddleRight =
            findPointOnCircle (12 * 60) (hour * 60 + minute + 20) 28 center
    in
    g []
        [ line
            [ x1 <| String.fromInt secondsHandEnd.x
            , y1 <| String.fromInt secondsHandEnd.y
            , x2 <| String.fromInt secondsHand.x
            , y2 <| String.fromInt secondsHand.y
            , stroke "black"
            , strokeWidth "1px"
            , stroke "red"
            ]
            []
        , polygon
            [ points <|
                String.fromInt center.x
                    ++ ","
                    ++ String.fromInt center.y
                    ++ " "
                    ++ String.fromInt minutesHandMiddleLeft.x
                    ++ ","
                    ++ String.fromInt minutesHandMiddleLeft.y
                    ++ " "
                    ++ String.fromInt minutesHand.x
                    ++ ","
                    ++ String.fromInt minutesHand.y
                    ++ " "
                    ++ String.fromInt minutesHandMiddleRight.x
                    ++ ","
                    ++ String.fromInt minutesHandMiddleRight.y
            , stroke "black"
            , strokeWidth "1px"
            ]
            []
        , polygon
            [ points <|
                String.fromInt center.x
                    ++ ","
                    ++ String.fromInt center.y
                    ++ " "
                    ++ String.fromInt hoursHandMiddleLeft.x
                    ++ ","
                    ++ String.fromInt hoursHandMiddleLeft.y
                    ++ " "
                    ++ String.fromInt hoursHand.x
                    ++ ","
                    ++ String.fromInt hoursHand.y
                    ++ " "
                    ++ String.fromInt hoursHandMiddleRight.x
                    ++ ","
                    ++ String.fromInt hoursHandMiddleRight.y
            , stroke "black"
            , strokeWidth "1px"
            ]
            []
        ]


drawNumber : Int -> Svg msg
drawNumber hour =
    let
        point =
            findPointOnCircle 12 hour 180 <| Point 300 300
    in
    Svg.text_
        [ alignmentBaseline "middle"
        , textAnchor "middle"
        , x <| String.fromInt point.x
        , y <| String.fromInt point.y
        ]
        [ Svg.text <| String.fromInt hour ]


findPointOnCircle : Int -> Int -> Int -> Point -> Point
findPointOnCircle slices slice radius center =
    let
        r =
            toFloat radius

        deg =
            -(toFloat slice * (degrees <| 360 / toFloat slices))
    in
    { x = floor (toFloat center.x + r * -(sin deg)), y = floor (toFloat center.y + r * -(cos deg)) }
