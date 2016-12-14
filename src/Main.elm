port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing (onClick, on, targetValue, onInput)
import Http
import Json.Decode as Json
import Json.Decode exposing ((:=))
import String
import Char exposing (isLower, isUpper)
import List
import Task
import Date exposing (..)
import Date.Extra.Format exposing (isoDateString)
import Date.Extra.Core exposing (toFirstOfMonth, lastOfPrevMonthDate, isoDayOfWeek)
import Date.Extra.Duration exposing (add, Duration(Week, Day))


port fetchFile : ( String, String, String ) -> Cmd msg


intDecoder : Json.Decoder Int
intDecoder =
    targetValue
        `Json.andThen`
            \val ->
                case String.toInt val of
                    Ok i ->
                        Json.succeed i

                    Err err ->
                        Json.fail err


type alias Flags =
    { token : String, apiUrl : String }


main : Program Flags
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias DateString =
    String



-- MODEL


type alias Project =
    { id : String
    , name : String
    , customer : String
    }


type alias StatusRange =
    { start : DateString
    , end : DateString
    }


type alias Model =
    { projects : List Project
    , statusRange : StatusRange
    , projectStatusRange : StatusRange
    , selectedProject : Maybe Project
    , token : String
    , apiUrl : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initialRange =
            StatusRange "1970-01-01" "1970-01-01"

        initialModel =
            Model [] initialRange initialRange Nothing flags.token flags.apiUrl
    in
        ( initialModel, Task.perform Initialize Initialize Date.now )



-- UPDATE


type Msg
    = FetchSucceed (List Project)
    | FetchFail Http.Error
    | Initialize Date.Date
    | RangeStartDate String
    | RangeEndDate String
    | ProjectRangeStartDate String
    | ProjectRangeEndDate String
    | SelectProject String
    | DateMissing
    | DownloadFile String String String


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Initialize date ->
            let
                currentWeekDay =
                    date |> Date.dayOfWeek |> isoDayOfWeek

                mondayOfPrevWeek =
                    date
                        |> add Day (1 - currentWeekDay)
                        -- find Monday of this week …
                        |>
                            add Week -1
                        -- … then subtract a week
                        |>
                            isoDateString

                sundayOfPrevWeek =
                    date
                        |> add Day (-currentWeekDay)
                        -- find Sunday of previous week
                        |>
                            isoDateString

                firstDayOfPrevMonth =
                    date
                        |> lastOfPrevMonthDate
                        |> toFirstOfMonth
                        |> isoDateString

                lastDayOfPrevMonth =
                    date
                        |> lastOfPrevMonthDate
                        |> isoDateString
            in
                ( { model
                    | statusRange = StatusRange mondayOfPrevWeek sundayOfPrevWeek
                    , projectStatusRange = StatusRange firstDayOfPrevMonth lastDayOfPrevMonth
                  }
                , getProjects model.token model.apiUrl
                )

        FetchSucceed projects ->
            ( { model | projects = projects, selectedProject = List.head projects }, Cmd.none )

        FetchFail _ ->
            ( model, Cmd.none )

        ProjectRangeStartDate start ->
            let
                oldRange =
                    model.projectStatusRange

                newRange =
                    { oldRange | start = start }
            in
                ( { model | projectStatusRange = newRange }, Cmd.none )

        ProjectRangeEndDate end ->
            let
                oldRange =
                    model.projectStatusRange

                newRange =
                    { oldRange | end = end }
            in
                ( { model | projectStatusRange = newRange }, Cmd.none )

        RangeStartDate start ->
            let
                oldRange =
                    model.statusRange

                newRange =
                    { oldRange | start = start }
            in
                ( { model | statusRange = newRange }, Cmd.none )

        RangeEndDate end ->
            let
                oldRange =
                    model.statusRange

                newRange =
                    { oldRange | end = end }
            in
                ( { model | statusRange = newRange }, Cmd.none )

        SelectProject projectId ->
            ( { model | selectedProject = List.filter (\x -> x.id == projectId) model.projects |> List.head }, Cmd.none )

        DateMissing ->
            ( model, Cmd.none )

        DownloadFile url jwt filename ->
            ( model, fetchFile ( url, jwt, filename ) )



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ status model, projects model ]


status : Model -> Html Msg
status model =
    let
        url =
            model.apiUrl
                ++ "/reporting/time_tracking_status"
                ++ "?start_date="
                ++ model.statusRange.start
                ++ "&end_date="
                ++ model.statusRange.end

        jwt =
            Http.uriDecode model.token

        filename =
            "status-" ++ model.statusRange.start ++ "–" ++ model.statusRange.end ++ ".csv"
    in
        div []
            [ h3 [] [ text "Timeføringstatus" ]
            , div [ class "mdl-grid" ]
                [ div [ class "mdl-cell mdl-cell--3-col mdl-cell--6-col-phone" ]
                    [ label [ for "start" ] [ text "Startdato" ]
                    , input [ id "start", type' "date", class "form-control", onInput RangeStartDate, value model.statusRange.start ] []
                    ]
                , div [ class "mdl-cell mdl-cell--3-col mdl-cell--6-col-phone" ]
                    [ label [ for "end" ] [ text "Sluttdato (inklusiv)" ]
                    , input [ id "end", type' "date", class "form-control", onInput RangeEndDate, value model.statusRange.end ] []
                    ]
                ]
            , div [ class "mdl-grid" ]
                [ div [ class "mdl-cell mdl-cell--2-col mdl-cell--6-col-phone" ]
                    [ button [ onClick (DownloadFile url jwt filename) ] [ text "Hent rapport" ] ]
                ]
            ]


projects : Model -> Html Msg
projects model =
    let
        toListItem p =
            option [ value p.id ] [ text (p.customer ++ " – " ++ p.name) ]

        items =
            (List.map toListItem model.projects)

        url =
            Maybe.map
                (\p ->
                    model.apiUrl
                        ++ "/reporting/hours/"
                        ++ p.id
                        ++ "?start_date="
                        ++ model.projectStatusRange.start
                        ++ "&end_date="
                        ++ model.projectStatusRange.end
                )
                model.selectedProject

        jwt =
            Http.uriDecode model.token

        filename =
            Maybe.map
                (\p ->
                    model.projectStatusRange.start
                        ++ "–"
                        ++ model.projectStatusRange.end
                        ++ "–"
                        ++ p.id
                        ++ "–"
                        ++ String.filter (\c -> isUpper c || isLower c) p.customer
                        ++ "–"
                        ++ String.filter (\c -> isUpper c || isLower c) p.name
                        ++ ".csv"
                )
                model.selectedProject
    in
        div []
            [ h3 [] [ text "Prosjekter" ]
            , div [ class "mdl-grid" ]
                [ div [ class "mdl-cell mdl-cell--3-col mdl-cell--6-col-phone" ]
                    [ label [ for "start" ] [ text "Startdato" ]
                    , input [ id "start", type' "date", class "form-control", onInput ProjectRangeStartDate, value model.projectStatusRange.start ] []
                    ]
                , div [ class "mdl-cell mdl-cell--3-col mdl-cell--6-col-phone" ]
                    [ label [ for "end" ] [ text "Sluttdato (inklusiv)" ]
                    , input [ id "end", type' "date", class "form-control", onInput ProjectRangeEndDate, value model.projectStatusRange.end ] []
                    ]
                ]
            , div [ class "mdl-grid" ]
                [ div [ class "mdl-cell mdl-cell--3-col mdl-cell--6-col-phone" ]
                    [ select [ onInput SelectProject ] items ]
                ]
            , div [ class "mdl-grid" ]
                [ div [ class "mdl-cell mdl-cell--2-col mdl-cell--6-col-phone" ]
                    (case ( url, filename ) of
                        ( Just url, Just filename ) ->
                            [ button [ onClick (DownloadFile url jwt filename) ] [ text "Hent rapport" ] ]

                        ( _, _ ) ->
                            [ button [ disabled True ] [ text "Hent rapport" ] ]
                    )
                ]
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getProjects : String -> String -> Cmd Msg
getProjects token apiUrl =
    let
        url =
            apiUrl ++ "/reporting/projects"

        request =
            { verb = "GET"
            , headers = [ ( "Authorization", "Bearer " ++ token ) ]
            , url = url
            , body = Http.empty
            }
    in
        Task.perform FetchFail FetchSucceed (Http.fromJson decodeProject (Http.send Http.defaultSettings request))


decodeProject : Json.Decoder (List Project)
decodeProject =
    Json.list (Json.object3 Project ("projectId" := Json.string) ("projectName" := Json.string) ("customerName" := Json.string))
