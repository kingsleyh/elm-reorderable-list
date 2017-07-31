effect module Reorderable.Mouse
    where { subscription = MySub }
    exposing
        ( Position
        , mouseEvent
        , mouseEventDecoder
        , MouseEvent
        , clicks
        , moves
        , downs
        , ups
        )

import Dict
import Dom.LowLevel as Dom
import Json.Decode as Json
import Process
import Task exposing (Task)


-- POSITIONS


type alias Position =
    { x : Int
    , y : Int
    }


type alias MouseEvent =
    { startingPosition : Position
    , movement : Position
    }


type alias Offset =
    { offsetLeft : Int
    , offsetTop : Int
    }


mouseEvent : Int -> Int -> Int -> Int -> List Offset -> MouseEvent
mouseEvent elemOffsetX elemOffsetY movementX movementY parentOffsets =
    let
        offsetLeft =
            List.map .offsetLeft parentOffsets |> List.sum

        offsetTop =
            List.map .offsetTop parentOffsets |> List.sum
    in
        { startingPosition = Position (elemOffsetX + offsetLeft) (elemOffsetY + offsetTop)
        , movement = Position movementX movementY
        }


mouseEventDecoder : Json.Decoder MouseEvent
mouseEventDecoder =
    Json.map5 mouseEvent
        (Json.at [ "srcElement", "offsetLeft" ] Json.int)
        (Json.at [ "srcElement", "offsetTop" ] Json.int)
        (Json.field "movementX" Json.int)
        (Json.field "movementY" Json.int)
        offsetsDecoder


offsetsDecoder : Json.Decoder (List Offset)
offsetsDecoder =
    let
        offsetDepthField : Int -> List String
        offsetDepthField depth =
            "srcElement" :: List.repeat depth "offsetParent"

        offsetParent : Int -> Json.Decoder (Maybe Offset)
        offsetParent depth =
            Json.maybe (Json.at (offsetDepthField depth) offsetDecoder)

        --|> Json.map (Debug.log ("offset at depth: " ++ (toString depth)))
        go : Int -> Json.Decoder (List Offset)
        go depth =
            offsetParent depth
                |> Json.andThen
                    (\maybeOffset ->
                        case maybeOffset of
                            Nothing ->
                                Json.succeed []

                            Just offset ->
                                Json.map2 (::) (Json.succeed offset) (go (depth + 1))
                    )
    in
        go 0


offsetDecoder : Json.Decoder Offset
offsetDecoder =
    Json.map2 Offset
        (Json.field "offsetLeft" Json.int)
        (Json.field "offsetTop" Json.int)



--        (Json.maybe "offsetParent" (Json.succeed Nothing))
-- MOUSE EVENTS


clicks : (MouseEvent -> msg) -> Sub msg
clicks tagger =
    subscription (MySub "click" tagger)


moves : (MouseEvent -> msg) -> Sub msg
moves tagger =
    subscription (MySub "mousemove" tagger)


downs : (MouseEvent -> msg) -> Sub msg
downs tagger =
    subscription (MySub "mousedown" tagger)


{-| Get a position whenever the user *releases* the mouse button.
-}
ups : (MouseEvent -> msg) -> Sub msg
ups tagger =
    subscription (MySub "mouseup" tagger)



-- SUBSCRIPTIONS


type MySub msg
    = MySub String (MouseEvent -> msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func (MySub category tagger) =
    MySub category (tagger >> func)



-- EFFECT MANAGER STATE


type alias State msg =
    Dict.Dict String (Watcher msg)


type alias Watcher msg =
    { taggers : List (MouseEvent -> msg)
    , pid : Process.Id
    }



-- CATEGORIZE SUBSCRIPTIONS


type alias SubDict msg =
    Dict.Dict String (List (MouseEvent -> msg))


categorize : List (MySub msg) -> SubDict msg
categorize subs =
    categorizeHelp subs Dict.empty


categorizeHelp : List (MySub msg) -> SubDict msg -> SubDict msg
categorizeHelp subs subDict =
    case subs of
        [] ->
            subDict

        (MySub category tagger) :: rest ->
            categorizeHelp rest <|
                Dict.update category (categorizeHelpHelp tagger) subDict


categorizeHelpHelp : a -> Maybe (List a) -> Maybe (List a)
categorizeHelpHelp value maybeValues =
    case maybeValues of
        Nothing ->
            Just [ value ]

        Just values ->
            Just (value :: values)



-- EFFECT MANAGER


init : Task Never (State msg)
init =
    Task.succeed Dict.empty


type alias Msg =
    { category : String
    , mouseEvent : MouseEvent
    }


(&>) t1 t2 =
    t1
        |> Task.andThen (\_ -> t2)


onEffects : Platform.Router msg Msg -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router newSubs oldState =
    let
        leftStep category { pid } task =
            Process.kill pid &> task

        bothStep category { pid } taggers task =
            task
                |> Task.andThen (\state -> Task.succeed (Dict.insert category (Watcher taggers pid) state))

        rightStep category taggers task =
            let
                tracker =
                    Dom.onDocument category mouseEventDecoder (Platform.sendToSelf router << Msg category)
            in
                task
                    |> Task.andThen
                        (\state ->
                            Process.spawn tracker
                                |> Task.andThen (\pid -> Task.succeed (Dict.insert category (Watcher taggers pid) state))
                        )
    in
        Dict.merge
            leftStep
            bothStep
            rightStep
            oldState
            (categorize newSubs)
            (Task.succeed Dict.empty)


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router { category, mouseEvent } state =
    case Dict.get category state of
        Nothing ->
            Task.succeed state

        Just { taggers } ->
            let
                send tagger =
                    Platform.sendToApp router (tagger mouseEvent)
            in
                Task.sequence (List.map send taggers)
                    &> Task.succeed state
