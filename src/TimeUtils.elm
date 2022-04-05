module TimeUtils exposing (..)

import Element exposing (Element)
import Element.Input
import Time



-- Time ----------------------------------------------------------------------------------------------------------------


type alias Milliseconds =
    Int


units =
    { second = 1000
    , minute = 60 * 1000
    , hour = 60 * 60 * 1000
    , day = 24 * 60 * 60 * 1000
    }


padZero : Int -> String
padZero i =
    let
        string =
            String.fromInt i

        len =
            String.length string

        pad =
            if len == 1 then
                "0"

            else
                ""
    in
    pad ++ string


showTimeWithSeconds : Time.Zone -> Time.Posix -> String
showTimeWithSeconds zone posix =
    String.join ":" <|
        List.map padZero
            [ Time.toHour zone posix
            , Time.toMinute zone posix
            , Time.toSecond zone posix
            ]


showTimeWithoutSeconds : Time.Zone -> Time.Posix -> String
showTimeWithoutSeconds zone posix =
    String.join ":" <|
        List.map padZero
            [ Time.toHour zone posix
            , Time.toMinute zone posix
            ]


addMs : Time.Posix -> Milliseconds -> Time.Posix
addMs posix milliseconds =
    Time.millisToPosix <|
        Time.posixToMillis posix
            + milliseconds


round10 : Time.Zone -> Time.Posix -> Time.Posix
round10 zone aTime =
    let
        currentMins =
            Time.toMinute zone aTime

        roundedCurrentMins =
            round (toFloat currentMins / 10.0) * 10
    in
    addMs aTime <| (roundedCurrentMins - currentMins) * units.minute


offset60 : Time.Zone -> Time.Posix -> Milliseconds
offset60 zone posix =
    let
        currentMins =
            Time.toMinute zone posix

        roundedCurrentMins =
            round (toFloat currentMins / 60.0) * 60
    in
    roundedCurrentMins - currentMins



-- EditTime ------------------------------------------------------------------------------------------------------------


type TimeUnit
    = Hours
    | Minutes
    | Seconds


timeUnitToMilliseconds : TimeUnit -> Milliseconds
timeUnitToMilliseconds u =
    case u of
        Hours ->
            60 * 60 * 1000

        Minutes ->
            60 * 1000

        Seconds ->
            1000


type alias HMS =
    { hours : Int, minutes : Int, seconds : Int }


posixToHMS : Time.Zone -> Time.Posix -> HMS
posixToHMS zone t =
    { hours = Time.toHour zone t
    , minutes = Time.toMinute zone t
    , seconds = Time.toSecond zone t
    }


hmsToPosix : Time.Zone -> HMS -> Time.Posix
hmsToPosix zone { hours, minutes, seconds } =
    let
        zero =
            Time.millisToPosix 0

        h =
            hours - Time.toHour zone zero

        m =
            minutes - Time.toMinute zone zero

        s =
            seconds - Time.toSecond zone zero

        ms =
            0 - Time.toMillis zone zero
    in
    Time.millisToPosix <|
        h
            * units.hour
            + m
            * units.minute
            + s
            * units.second
            + ms


justTheTime : Time.Zone -> Time.Posix -> Time.Posix
justTheTime zone =
    posixToHMS zone >> hmsToPosix zone


timeZoneOffsetMillis : Time.Zone -> Int
timeZoneOffsetMillis zone =
    let
        zero =
            Time.millisToPosix 0

        hours =
            Time.toHour zone zero

        minutes =
            Time.toMinute zone zero

        seconds =
            Time.toSecond zone zero

        millis =
            Time.toMillis zone zero
    in
    hours
        * units.hour
        + minutes
        * units.minute
        + seconds
        * units.second
        + millis



-- Integer Input -------------------------------------------------------------------------------------------------------


inputPositiveIntegerBelow : Int -> List (Element.Attribute (Maybe Int)) -> Maybe Int -> Element (Maybe Int)
inputPositiveIntegerBelow max attrs current =
    let
        inRange string =
            case String.toInt string of
                Just i ->
                    if i >= 0 && i < max then
                        Just i

                    else
                        current

                Nothing ->
                    Nothing
    in
    Element.Input.text attrs
        { onChange = inRange
        , text =
            case current of
                Nothing ->
                    ""

                Just i ->
                    padZero i
        , placeholder = Nothing
        , label = Element.Input.labelHidden <| "Number between 0 and " ++ String.fromInt max
        }



{-
   type EditIntMsg unit
       = AddInt unit Int
       | SetInt unit Int
       | ErrInt unit String


   resultStringIntToEditIntMsg : unit -> Result String Int -> EditIntMsg unit
   resultStringIntToEditIntMsg unit r =
       case r of
           Ok newInt ->
               SetInt unit newInt

           Err string ->
               ErrInt unit string


   resultStringIntToString : Result String Int -> String
   resultStringIntToString r =
       case r of
           Ok value ->
               String.fromInt value

           Err error ->
               error


   updateTime : Time.Zone -> Time.Posix -> EditIntMsg TimeUnit -> Time.Posix
   updateTime zone t msg =
       case msg of
           AddInt timeUnit int ->
               addMs t (timeUnitToMilliseconds timeUnit * int)

           SetInt timeUnit int ->
               let
                   { hours, minutes, seconds } =
                       posixToHMS zone t
               in
               hmsToPosix <|
                   case timeUnit of
                       Hours ->
                           { hours = int, minutes = minutes, seconds = seconds }

                       Minutes ->
                           { hours = hours, minutes = int, seconds = seconds }

                       Seconds ->
                           { hours = hours, minutes = minutes, seconds = int }

           ErrInt unit string ->
               t


   editTime : Time.Zone -> Time.Posix -> Element Time.Posix
   editTime zone t =
       let
           { hours, minutes, seconds } =
               posixToHMS zone t
       in
       Element.map (updateTime zone t) <|
           Element.row
               []
               [ editInt Hours 23 hours
               , Element.text ":"
               , editInt Minutes 59 minutes
               , Element.text ":"
               , editInt Seconds 59 seconds
               ]
-}
