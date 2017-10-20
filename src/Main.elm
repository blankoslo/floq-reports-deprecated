port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, disabled, for, id, type_, value)
import Html.Events exposing (onClick, on, targetValue, onInput)
import Http
import Json.Decode exposing (Decoder, list, string)
import Json.Decode.Pipeline exposing (decode, required)
import Char exposing (isLower, isUpper)
import Task
import Date exposing (Date)
import Date.Extra.Format exposing (isoDateString)
import Date.Extra.Core exposing (toFirstOfMonth, lastOfPrevMonthDate, isoDayOfWeek)
import Date.Extra.Duration exposing (add, Duration(Week, Day))


port fetchFile : ( String, String, String ) -> Cmd msg


type alias Flags =
    { token : String, apiUrl : String }


main : Program Flags Model Msg
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
    , visibilityStatusRange : StatusRange
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
            Model [] initialRange initialRange initialRange Nothing flags.token flags.apiUrl
    in
        initialModel ! [ Task.perform SetDate Date.now, getProjects flags.token flags.apiUrl ]



-- UPDATE


type Msg
    = LoadedProjects (Result Http.Error (List Project))
    | SetDate Date
    | RangeStartDate String
    | RangeEndDate String
    | ProjectRangeStartDate String
    | ProjectRangeEndDate String
    | VisibilityStartDate String
    | VisibilityEndDate String
    | SelectProject String
    | DateMissing
    | DownloadFile String String String



update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        SetDate date ->
            let
                currentWeekDay =
                    date |> Date.dayOfWeek |> isoDayOfWeek

                mondayOfPrevWeek =
                    date
                        -- find Monday of this week …
                        |> add Day (1 - currentWeekDay)
                        -- … then subtract a week
                        |> add Week -1
                        |> isoDateString

                sundayOfPrevWeek =
                    date
                        -- find Sunday of previous week
                        |> add Day (-currentWeekDay)
                        |> isoDateString

                firstDayOfPrevMonth =
                    date
                        |> lastOfPrevMonthDate
                        |> toFirstOfMonth
                        |> isoDateString

                lastDayOfPrevMonth =
                    date
                        |> lastOfPrevMonthDate
                        |> isoDateString
                today = 
                    date
                        |> isoDateString
            in
                ( { model
                    | statusRange = StatusRange mondayOfPrevWeek sundayOfPrevWeek
                    , projectStatusRange = StatusRange firstDayOfPrevMonth lastDayOfPrevMonth
                    , visibilityStatusRange = StatusRange mondayOfPrevWeek today
                  }
                , Cmd.none
                )

        LoadedProjects (Ok projects) ->
            ( { model | projects = projects, selectedProject = List.head projects }, Cmd.none )

        LoadedProjects (Err _) ->
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

        VisibilityStartDate start -> 
            let
                oldRange =
                    model.visibilityStatusRange

                newRange =
                    { oldRange | start = start }
            in
                ( { model | visibilityStatusRange = newRange }, Cmd.none )

        VisibilityEndDate end -> 
            let
                oldRange =
                    model.visibilityStatusRange

                newRange =
                    { oldRange | end = end }
            in
                ( { model | visibilityStatusRange = newRange }, Cmd.none )


-- VIEW


view : Model -> Html Msg
view model =
    div [] [ status model, projects model, visibility model ]


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
            Http.encodeUri model.token

        filename =
            "status-" ++ model.statusRange.start ++ "–" ++ model.statusRange.end ++ ".csv"
    in
        div []
            [ h3 [] [ text "Timeføringstatus" ]
            , div [ class "mdl-grid" ]
                [ div [ class "mdl-cell mdl-cell--3-col mdl-cell--6-col-phone" ]
                    [ label [ for "start" ] [ text "Startdato" ]
                    , input [ id "start", type_ "date", class "form-control", onInput RangeStartDate, value model.statusRange.start ] []
                    ]
                , div [ class "mdl-cell mdl-cell--3-col mdl-cell--6-col-phone" ]
                    [ label [ for "end" ] [ text "Sluttdato (inklusiv)" ]
                    , input [ id "end", type_ "date", class "form-control", onInput RangeEndDate, value model.statusRange.end ] []
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
            Http.encodeUri model.token

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
                    , input [ id "start", type_ "date", class "form-control", onInput ProjectRangeStartDate, value model.projectStatusRange.start ] []
                    ]
                , div [ class "mdl-cell mdl-cell--3-col mdl-cell--6-col-phone" ]
                    [ label [ for "end" ] [ text "Sluttdato (inklusiv)" ]
                    , input [ id "end", type_ "date", class "form-control", onInput ProjectRangeEndDate, value model.projectStatusRange.end ] []
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


visibility : Model -> Html Msg
visibility model = 
    let
        url =
            model.apiUrl
                ++ "/reporting/visibility/"
                ++ "?start_date="
                ++ model.visibilityStatusRange.start
                ++ "&end_date="
                ++ model.visibilityStatusRange.end

        jwt =
            Http.encodeUri model.token

        filename =
            model.projectStatusRange.start
                ++ "–"
                ++ model.projectStatusRange.end
                ++ "–"
                ++ "visibility"
                ++ ".csv"
    in
        div []
            [ h3 [] [ text "Visibility" ]
            , div [ class "mdl-grid" ]
                [ div [ class "mdl-cell mdl-cell--3-col mdl-cell--6-col-phone" ]
                    [ label [ for "start" ] [ text "Startdato" ]
                    , input [ id "start", type_ "date", class "form-control", onInput VisibilityStartDate, value model.visibilityStatusRange.start ] []
                    ]
                , div [ class "mdl-cell mdl-cell--3-col mdl-cell--6-col-phone" ]
                    [ label [ for "end" ] [ text "Sluttdato (inklusiv)" ]
                    , input [ id "end", type_ "date", class "form-control", onInput VisibilityEndDate, value model.visibilityStatusRange.end ] []
                    ]
                ]
            , div [ class "mdl-grid" ]
                [ div [ class "mdl-cell mdl-cell--2-col mdl-cell--6-col-phone" ]
                    [ button [ onClick (DownloadFile url jwt filename) ] [ text "Hent rapport" ] ]
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
            { method = "GET"
            , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
            , url = url
            , body = Http.emptyBody
            , expect = Http.expectJson decodeProject
            , timeout = Nothing
            , withCredentials = False
            }
    in
        Http.send LoadedProjects <| Http.request request


decodeProject : Decoder (List Project)
decodeProject =
    decode Project
        |> required "projectId" string
        |> required "projectName" string
        |> required "customerName" string
        |> list
