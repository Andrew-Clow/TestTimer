module HHMM exposing (..)

import Time


type HHMM a
    = HHMM a a


hours : HHMM a -> a
hours (HHMM hh _) =
    hh


minutes : HHMM a -> a
minutes (HHMM _ mm) =
    mm


fromPosix : Time.Zone -> Time.Posix -> HHMM Int
fromPosix zone time =
    HHMM (Time.toHour zone time) (Time.toMinute zone time)


add : HHMM Int -> HHMM Int -> HHMM Int
add (HHMM h m) (HHMM hh mm) =
    wrap <| HHMM (h + hh) (m + mm)


addString : HHMM Int -> HHMM String -> HHMM String
addString offset value =
    case toInts value of
        Nothing ->
            offset |> wrap |> toStrings

        Just val ->
            add offset val |> toStrings


toStrings : HHMM Int -> HHMM String
toStrings input =
    map toTwoDigitString input


toInts : HHMM String -> Maybe (HHMM Int)
toInts input =
    case map String.toInt input of
        HHMM (Just hh) (Just mm) ->
            Just <| wrap (HHMM hh mm)

        _ ->
            Nothing


map : (a -> b) -> HHMM a -> HHMM b
map f (HHMM hh mm) =
    HHMM (f hh) (f mm)


toTwoDigitString : Int -> String
toTwoDigitString i =
    let
        string =
            i |> String.fromInt
    in
    if String.length string < 2 then
        "0" ++ string

    else
        string


wrap : HHMM Int -> HHMM Int
wrap (HHMM hh mm) =
    let
        mins =
            modBy 60 mm

        hrs =
            modBy 24 <|
                if mm < 0 then
                    hh - 1

                else if mm > 59 then
                    hh + 1

                else
                    hh
    in
    HHMM hrs mins
