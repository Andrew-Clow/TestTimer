module TestTimer exposing (..)

import AnalogClock exposing (view)
import Browser
import Button exposing (Content(..))
import Colour
import Element exposing (Element)
import Element.Input
import Html exposing (Html)
import Size
import Task
import Time
import Time.Extra
import TimeUtils
import UndoList exposing (UndoList)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Msg -----------------------------------------------------------------------------------------------------------------


type Msg
    = Tick Time.Posix
    | SetTimeZone Time.Zone
    | InitialiseTime Time.Posix
    | NewTest TestEditing
    | CopyTest Test
    | UndoLast
    | RedoLast
    | DeleteTest Int
    | AddTest


type TestEditing
    = TypedName String
    | EditedStartHours (Maybe Int)
    | EditedStartMinutes (Maybe Int)
    | EditedLength (Maybe Int)
    | ToggleExtraTime Bool
    | SaveTest (List Test)
    | CancelNewTest



-- Model ---------------------------------------------------------------------------------------------------------------


type alias Model =
    { timeZone : Time.Zone
    , currentTime : Time.Posix
    , tests : UndoList (List Test)
    , newTest : Maybe NewTestUnderEditing
    }


type alias TimeData r =
    { r
        | timeZone : Time.Zone
        , currentTime : Time.Posix
    }



-- Tests ---------------------------------------------------------------------------------------------------------------


type alias Test =
    { name : String
    , startTime : Time.Posix
    , lengthInMinutes : Int
    , status : Maybe TestStatus
    }


type TestStatus
    = NotStarted
    | InProgress
    | Completed


type alias NewTestUnderEditing =
    { name : String
    , startHours : Maybe Int
    , startMinutes : Maybe Int
    , length : Maybe Int
    , addExtraTimeToo : Bool
    }


duplicateTest : { r | timeZone : Time.Zone, currentTime : Time.Posix } -> Test -> NewTestUnderEditing
duplicateTest model test =
    let
        hms =
            TimeUtils.posixToHMS model.timeZone <| afterMinutes 1 model
    in
    { name = test.name
    , startHours = Just hms.hours
    , startMinutes = Just hms.minutes
    , length = Just test.lengthInMinutes
    , addExtraTimeToo = False
    }


setTimeToday : Time.Zone -> Time.Posix -> Int -> Int -> Time.Posix
setTimeToday zone currentTime hh mm =
    let
        parts =
            Time.Extra.posixToParts zone currentTime

        newTime =
            { parts | hour = hh, minute = mm, second = 0, millisecond = 0 }
    in
    Time.Extra.partsToPosix zone newTime


finishTime : Test -> Time.Posix
finishTime test =
    TimeUtils.addMs test.startTime (TimeUtils.units.minute * test.lengthInMinutes)


extraTimeVersion : Test -> Test
extraTimeVersion test =
    { name = test.name ++ " (extra time)"
    , startTime = test.startTime
    , lengthInMinutes = ceiling <| toFloat test.lengthInMinutes * 1.25
    , status = Nothing
    }


earliestFinish : Test -> Test -> Order
earliestFinish one two =
    case compare (Time.posixToMillis <| finishTime one) (Time.posixToMillis <| finishTime two) of
        GT ->
            GT

        LT ->
            LT

        EQ ->
            case compare (Time.posixToMillis one.startTime) (Time.posixToMillis two.startTime) of
                GT ->
                    GT

                LT ->
                    LT

                EQ ->
                    compare one.name two.name


sortTests : List Test -> List Test
sortTests =
    List.sortWith earliestFinish


makeTests : TimeData r -> NewTestUnderEditing -> Maybe (List Test)
makeTests timeData { name, startHours, startMinutes, length, addExtraTimeToo } =
    case ( startHours, startMinutes, length ) of
        ( Just h, Just m, Just l ) ->
            if name == "" then
                Nothing

            else
                Just <|
                    let
                        test =
                            { name = name
                            , startTime =
                                setTimeToday timeData.timeZone timeData.currentTime h m

                            --TimeUtils.hmsToPosix timeData.timeZone { hours = h, minutes = m, seconds = 0 }
                            , lengthInMinutes = l
                            , status = Nothing
                            }
                    in
                    if addExtraTimeToo then
                        [ test, extraTimeVersion test ]

                    else
                        [ test ]

        _ ->
            Nothing


afterMinutes : Int -> { r | currentTime : Time.Posix, timeZone : Time.Zone } -> Time.Posix
afterMinutes n model =
    TimeUtils.addMs model.currentTime (n * TimeUtils.units.minute)


defaultTest : { r | currentTime : Time.Posix, timeZone : Time.Zone } -> NewTestUnderEditing
defaultTest model =
    let
        startTime =
            afterMinutes 5 model
    in
    { name = ""
    , startHours = Just <| Time.toHour model.timeZone startTime
    , startMinutes = Just <| Time.toMinute model.timeZone startTime
    , length = Just 60
    , addExtraTimeToo = True
    }



-- init ----------------------------------------------------------------------------------------------------------------


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { timeZone = Time.utc
            , currentTime = Time.millisToPosix 0
            , tests = UndoList.fresh []
            , newTest = Nothing
            }
    in
    ( model
    , Cmd.batch
        [ Task.perform SetTimeZone Time.here
        , Task.perform InitialiseTime Time.now
        ]
    )



-- update --------------------------------------------------------------------------------------------------------------


updateTests : Model -> (List Test -> List Test) -> Model
updateTests model f =
    { model | tests = UndoList.new (f model.tests.present) model.tests }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                Tick posix ->
                    { model | currentTime = posix, tests = UndoList.mapPresent (List.map <| updateStatus model) model.tests }

                SetTimeZone zone ->
                    { model | timeZone = zone }

                InitialiseTime posix ->
                    { model | currentTime = posix }

                NewTest newTestMsg ->
                    updateNewTest newTestMsg model

                AddTest ->
                    case model.newTest of
                        Nothing ->
                            { model | newTest = Just (defaultTest model) }

                        Just _ ->
                            model

                CopyTest test ->
                    case model.newTest of
                        Nothing ->
                            { model | newTest = Just (duplicateTest model test) }

                        Just _ ->
                            model

                DeleteTest index ->
                    updateTests model (deleteAt index)

                UndoLast ->
                    { model | tests = model.tests |> UndoList.undo }

                RedoLast ->
                    { model | tests = model.tests |> UndoList.redo }
    in
    ( newModel, Cmd.none )


status : TimeData r -> Test -> TestStatus
status model test =
    let
        start =
            test.startTime |> Time.posixToMillis

        now =
            model.currentTime |> Time.posixToMillis

        finish =
            finishTime test |> Time.posixToMillis
    in
    if now < start then
        NotStarted

    else if now < finish then
        InProgress

    else
        Completed


showStatus : TestStatus -> Element msg
showStatus ts =
    case ts of
        NotStarted ->
            button Pause (Info Grey) Nothing

        InProgress ->
            button Go (Info Green) Nothing

        Completed ->
            button Stop (Info Red) Nothing


updateStatus : TimeData r -> Test -> Test
updateStatus m t =
    { t | status = Just <| status m t }


deleteAt : Int -> List a -> List a
deleteAt index list =
    List.take index list
        ++ List.drop (index + 1) list


asNewTestIn : Model -> NewTestUnderEditing -> Model
asNewTestIn m t =
    { m | newTest = Just t }


incorporateTestsAndCancelNewTest : List Test -> Model -> Model
incorporateTestsAndCancelNewTest newTests model =
    updateTests { model | newTest = Nothing } ((++) newTests >> sortTests)



--{ model | tests = sortTests <| model.tests ++ newTests, newTest = Nothing }


updateNewTest : TestEditing -> Model -> Model
updateNewTest msg model =
    case model.newTest of
        Nothing ->
            model

        Just newTest ->
            case msg of
                TypedName string ->
                    { newTest | name = string } |> asNewTestIn model

                EditedStartHours sh ->
                    { newTest | startHours = sh } |> asNewTestIn model

                EditedStartMinutes sm ->
                    { newTest | startMinutes = sm } |> asNewTestIn model

                EditedLength maybeInt ->
                    { newTest | length = maybeInt } |> asNewTestIn model

                ToggleExtraTime et ->
                    { newTest | addExtraTimeToo = et } |> asNewTestIn model

                SaveTest newTests ->
                    incorporateTestsAndCancelNewTest newTests model

                CancelNewTest ->
                    { model | newTest = Nothing }



-- Time ----------------------------------------------------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every TimeUtils.units.second Tick



-- view ----------------------------------------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    Element.layout
        [ Size.padding.large
        , Element.width Element.fill
        ]
    <|
        Element.column
            [ Size.spacing.large
            ]
            [ Element.row [ Element.width <| Element.px 1100 ]
                [ Element.el [ Size.fontSize.enormous, Element.centerX ] <|
                    Element.text <|
                        TimeUtils.showTimeWithSeconds model.timeZone model.currentTime
                ]
            , Element.row [ Element.width Element.fill ]
                [ AnalogClock.view model
                , Element.column [ Size.spacing.large ]
                    [ viewTests model <| model.tests.present
                    , viewTestBeingAdded model model.newTest
                    ]
                ]
            ]


viewTests : { r | timeZone : Time.Zone, newTest : Maybe NewTestUnderEditing, currentTime : Time.Posix } -> List Test -> Element Msg
viewTests model tests =
    Element.indexedTable
        [ Size.spacing.large
        , Size.fontSize.huge
        ]
        { data = tests
        , columns =
            [ { header = Element.none
              , width = Element.shrink
              , view = \_ test -> test |> status model |> showStatus
              }
            , { header = Element.text "Test"
              , width = Element.shrink
              , view =
                    \_ test ->
                        Element.el [ Element.centerY ] <|
                            Element.text test.name
              }
            , { header = Element.text "Start"
              , width = Element.shrink
              , view =
                    \_ test ->
                        Element.el [ Element.centerY ] <|
                            Element.text <|
                                TimeUtils.showTimeWithoutSeconds model.timeZone test.startTime
              }
            , { header = Element.text "Finish"
              , width = Element.shrink
              , view =
                    \_ test ->
                        Element.el [ Element.centerY ] <|
                            Element.text <|
                                TimeUtils.showTimeWithoutSeconds model.timeZone <|
                                    finishTime test
              }
            , { header = Element.none
              , width = Element.shrink
              , view = ifNotCurrentlyEditing model duplicateButton
              }
            , { header = Element.none
              , width = Element.shrink
              , view = ifNotCurrentlyEditing model deleteButton
              }
            ]
        }


ifNotCurrentlyEditing : { r | newTest : Maybe NewTestUnderEditing } -> (Int -> Test -> Element Msg) -> Int -> Test -> Element Msg
ifNotCurrentlyEditing model viewTest index test =
    case model.newTest of
        Nothing ->
            viewTest index test

        Just _ ->
            Element.none


duplicateButton : Int -> Test -> Element Msg
duplicateButton _ existingTest =
    button Copy Small (Just <| CopyTest existingTest)


deleteButton : Int -> Test -> Element Msg
deleteButton index _ =
    button Delete SmallRed (Just <| DeleteTest index)


twoDigitWidth : Element.Attribute msg
twoDigitWidth =
    Element.width <| Element.px 50


threeDigitWidth : Element.Attribute msg
threeDigitWidth =
    Element.width <| Element.px 75


ifTrueJust : a -> Bool -> Maybe a
ifTrueJust a true =
    if true then
        Just a

    else
        Nothing


viewTestBeingAdded : Model -> Maybe NewTestUnderEditing -> Element Msg
viewTestBeingAdded model mn =
    case mn of
        Nothing ->
            Element.row [ Size.spacing.large ] <|
                [ button Add Large (Just AddTest)
                , button Undo Large (UndoList.hasPast model.tests |> ifTrueJust UndoLast)
                , button Redo Large (UndoList.hasFuture model.tests |> ifTrueJust RedoLast)
                ]

        Just newTest ->
            Element.column
                [ Size.spacing.normal ]
                [ Element.Input.text [ Element.width <| Element.px 300 ]
                    { onChange = NewTest << TypedName
                    , text = newTest.name
                    , placeholder = Size.placeholderText "Type a name for the test"
                    , label = Element.Input.labelLeft [] <| Element.text "Test name: "
                    }
                , Element.row []
                    [ Element.text "Start time: "
                    , Element.map (NewTest << EditedStartHours) <|
                        TimeUtils.inputPositiveIntegerBelow 24 [ twoDigitWidth ] newTest.startHours
                    , Element.text " : "
                    , Element.map (NewTest << EditedStartMinutes) <|
                        TimeUtils.inputPositiveIntegerBelow 60 [ twoDigitWidth ] newTest.startMinutes
                    ]
                , Element.row []
                    [ Element.text "Length: "
                    , Element.map (NewTest << EditedLength) <|
                        TimeUtils.inputPositiveIntegerBelow (24 * 60) [ threeDigitWidth ] newTest.length
                    ]
                , Element.Input.checkbox []
                    { onChange = NewTest << ToggleExtraTime
                    , icon = Element.Input.defaultCheckbox
                    , checked = newTest.addExtraTimeToo
                    , label = Element.Input.labelLeft [] <| Element.text "Add a version with extra time?"
                    }
                , Element.row [ Size.spacing.normal ]
                    [ button Save Text <| Maybe.map (NewTest << SaveTest) <| makeTests model newTest
                    , button Cancel Text <| Just <| NewTest CancelNewTest
                    ]
                ]



-- Buttons -------------------------------------------------------------------------------------------------------------


type ButtonStyle
    = Large
    | Small
    | SmallRed
    | Text
    | Info Colour


type Colour
    = Grey
    | Red
    | Green


defaultButtonStyle : Button.Style
defaultButtonStyle =
    { enabled = Colour.natural.paleBlue
    , disabled = Colour.natural.veryLightGrey
    , borderRounding = 10
    , mouseOver = Just Colour.natural.appleWhite
    , size = Size.button.large
    }


getButtonStyle : ButtonStyle -> Button.Style
getButtonStyle i =
    case i of
        Large ->
            defaultButtonStyle

        Small ->
            { defaultButtonStyle
                | size = Size.button.normal
            }

        SmallRed ->
            { defaultButtonStyle
                | size = Size.button.normal
                , mouseOver = Just Colour.natural.pink
            }

        Text ->
            { defaultButtonStyle
                | size = Size.generousFont.normal
                , mouseOver = Just Colour.natural.blue
            }

        Info colour ->
            { defaultButtonStyle
                | mouseOver = Nothing
                , disabled =
                    case colour of
                        Red ->
                            Colour.natural.red

                        Grey ->
                            Colour.natural.midGrey

                        Green ->
                            Colour.natural.green
            }


type Button
    = Undo
    | Redo
    | Add
    | Copy
    | Delete
    | Save
    | Cancel
    | Pause
    | Stop
    | Go


content : Button -> Button.Content msg
content btn =
    case btn of
        Undo ->
            Button.icon.undo

        Redo ->
            Button.icon.redo

        Add ->
            Button.icon.add

        Copy ->
            Button.icon.copy

        Delete ->
            Button.icon.cross

        Save ->
            Button.Text "Save"

        Cancel ->
            Button.Text "Cancel"

        Pause ->
            Button.icon.pause

        Stop ->
            Button.icon.stop

        Go ->
            Button.icon.play


button : Button -> ButtonStyle -> Maybe msg -> Element msg
button =
    Button.makeButton getButtonStyle content
