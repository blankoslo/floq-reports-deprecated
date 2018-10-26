port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, disabled, for, id, type_, value)
import Html.Events exposing (onClick, on, targetValue, onInput)
import Http
import Json.Decode exposing (Decoder, list, string, int, maybe, nullable)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Char exposing (isLower, isUpper)
import Task
import Date exposing (Date)
import Date.Extra.Format exposing (isoDateString)
import Date.Extra.Core exposing (toFirstOfMonth, lastOfPrevMonthDate, isoDayOfWeek)
import Date.Extra.Duration exposing (add, Duration(Week, Day))


port fetchFile : ( String, String, String ) -> Cmd msg
port fetchEmployeeHoursFile : (String, String) -> Cmd msg 


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


type alias Employee = 
    { firstName : String
    , lastName : String
    , id : Int
    , terminationDate: Maybe String
    }

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
    , employees: List Employee
    , statusRange : StatusRange
    , projectStatusRange : StatusRange
    , employeeHoursRange : StatusRange
    , selectedProject : Maybe Project
    , selectedEmployee: Maybe Employee
    , token : String
    , apiUrl : String
    , now : Maybe Date
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initialRange =
            StatusRange "1970-01-01" "1970-01-01"

        initialModel =
            Model [] [] initialRange initialRange initialRange Nothing Nothing flags.token flags.apiUrl Nothing
    in
        initialModel ! [ Task.perform SetDate Date.now, getProjects flags.token flags.apiUrl, getEmployees flags.token flags.apiUrl ]



-- UPDATE


type Msg
    = SetProjects (Result Http.Error (List Project))
    | SetEmployees (Result Http.Error (List Employee))
    | SetDate Date
    | SetRangeStartDate String
    | SetRangeEndDate String
    | SetProjectRangeStartDate String
    | SetProjectRangeEndDate String
    | SetEmployeeHoursStartDate String
    | SetEmployeeHoursEndDate String
    | SelectProject String
    | SelectEmployee String
    | DateMissing
    | DownloadFile String String String
    | DownloadEmployeeHoursFile String String



hasQuitted : Maybe String -> Maybe Date -> Bool
hasQuitted terminationDate now = 
    case (terminationDate, now) of
        (Just terminationDate, Just now) ->
             case (Date.fromString terminationDate) of 
                (Ok date) -> Date.toTime date < Date.toTime now
                (Err _) -> False
        ( _, _ ) -> 
            False 

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
            in
                ( { model
                    | statusRange = StatusRange mondayOfPrevWeek sundayOfPrevWeek
                    , projectStatusRange = StatusRange firstDayOfPrevMonth lastDayOfPrevMonth
                    , employeeHoursRange = StatusRange firstDayOfPrevMonth lastDayOfPrevMonth
                    , now = Just date
                  }
                , Cmd.none
                )

        SetProjects (Ok projects) ->
            ( { model | projects = projects, selectedProject = List.head projects }, Cmd.none )

        SetProjects (Err _) ->
            ( model, Cmd.none )

        SetEmployees (Ok employees) ->
            let
                sortedEmployees = employees
                    |> List.map (\employee ->
                        case (employee.terminationDate) of
                            (Just date) -> 
                                if hasQuitted employee.terminationDate model.now
                                then { employee | firstName = "🚶‍♀️" ++ employee.firstName }
                                else employee 
                            (Nothing) -> employee 
                    )
                    |> List.sortBy .firstName
            in  
                ( { model | employees = sortedEmployees }, Cmd.none )
        
        SetEmployees (Err message) ->
            ( { model | employees = [{ firstName = toString message, lastName = "ERROR", id = 2, terminationDate = Nothing  }] } , Cmd.none )

        SetProjectRangeStartDate start ->
            let
                oldRange =
                    model.projectStatusRange

                newRange =
                    { oldRange | start = start }
            in
                ( { model | projectStatusRange = newRange }, Cmd.none )

        SetProjectRangeEndDate end ->
            let
                oldRange =
                    model.projectStatusRange

                newRange =
                    { oldRange | end = end }
            in
                ( { model | projectStatusRange = newRange }, Cmd.none )

        SetRangeStartDate start ->
            let
                oldRange =
                    model.statusRange

                newRange =
                    { oldRange | start = start }
            in
                ( { model | statusRange = newRange }, Cmd.none )

        SetRangeEndDate end ->
            let
                oldRange =
                    model.statusRange

                newRange =
                    { oldRange | end = end }
            in
                ( { model | statusRange = newRange }, Cmd.none )

        SetEmployeeHoursStartDate start ->
            let
                oldRange =
                    model.employeeHoursRange

                newRange =
                    { oldRange | start = start }
            in
                ( { model | employeeHoursRange = newRange }, Cmd.none )

        SetEmployeeHoursEndDate end ->
            let
                oldRange =
                    model.employeeHoursRange

                newRange =
                    { oldRange | end = end }
            in
                ( { model | employeeHoursRange = newRange }, Cmd.none )

        SelectProject projectId ->
            ( { model | selectedProject = List.filter (\x -> x.id == projectId) model.projects |> List.head }, Cmd.none )

        SelectEmployee employeeId -> 
            ( { model | selectedEmployee = List.filter (\e -> toString e.id == employeeId) model.employees |> List.head }, Cmd.none )


        DateMissing ->
            ( model, Cmd.none )

        DownloadFile url jwt filename ->
            ( model, fetchFile ( url, jwt, filename ) )
        DownloadEmployeeHoursFile filename payload ->
            ( model, fetchEmployeeHoursFile ( filename, payload ) )


-- VIEW


view : Model -> Html Msg
view model =
    div [] [ statusForm model, projectsForm model, employeesForm model ]


statusForm : Model -> Html Msg
statusForm model =
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
                    , input [ id "start", type_ "date", class "form-control", onInput SetRangeStartDate, value model.statusRange.start ] []
                    ]
                , div [ class "mdl-cell mdl-cell--3-col mdl-cell--6-col-phone" ]
                    [ label [ for "end" ] [ text "Sluttdato (inklusiv)" ]
                    , input [ id "end", type_ "date", class "form-control", onInput SetRangeEndDate, value model.statusRange.end ] []
                    ]
                ]
            , div [ class "mdl-grid" ]
                [ div [ class "mdl-cell mdl-cell--2-col mdl-cell--6-col-phone" ]
                    [ button [ onClick (DownloadFile url jwt filename) ] [ text "Hent rapport" ] ]
                ]
            ]


projectsForm : Model -> Html Msg
projectsForm model =
    let
        createOptionNode p =
            option [ value p.id ] [ text (p.customer ++ " – " ++ p.name) ]

        items =
            List.map createOptionNode model.projects

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
                    , input [ id "start", type_ "date", class "form-control", onInput SetProjectRangeStartDate, value model.projectStatusRange.start ] []
                    ]
                , div [ class "mdl-cell mdl-cell--3-col mdl-cell--6-col-phone" ]
                    [ label [ for "end" ] [ text "Sluttdato (inklusiv)" ]
                    , input [ id "end", type_ "date", class "form-control", onInput SetProjectRangeEndDate, value model.projectStatusRange.end ] []
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

employeesForm : Model -> Html Msg
employeesForm model = 
    let 
        createOptionNode p = option [ value ( toString p.id ) ] [ text (p.firstName ++ " " ++ p.lastName) ]
        employeeOptions = List.map createOptionNode model.employees
        url = model.apiUrl ++ "/rpc/entries_sums_for_employee_with_project"
        payload =
            Maybe.map
                (\employee ->
                    toString employee.id
                        ++ ","
                        ++ model.employeeHoursRange.start
                        ++ ","
                        ++ model.employeeHoursRange.end
                )
                model.selectedEmployee
        jwt = Http.encodeUri model.token
        filename =
            Maybe.map
                (\employee ->
                    model.employeeHoursRange.start
                        ++ "–"
                        ++ model.employeeHoursRange.end
                        ++ "–"
                        ++ employee.firstName
                        ++ "-"
                        ++ employee.lastName
                )
                model.selectedEmployee
    in
        div []
            [ h3 [] [ text "Timer ført av ansatt" ]
            , div [ class "mdl-grid" ]
                [ div [ class "mdl-cell mdl-cell--3-col mdl-cell--6-col-phone" ]
                    [ label [ for "start" ] [ text "Startdato" ]
                    , input [ id "start", type_ "date", class "form-control", onInput SetEmployeeHoursStartDate, value model.employeeHoursRange.start ] []
                    ]
                , div [ class "mdl-cell mdl-cell--3-col mdl-cell--6-col-phone" ]
                    [ label [ for "end" ] [ text "Sluttdato (inklusiv)" ]
                    , input [ id "end", type_ "date", class "form-control", onInput SetEmployeeHoursEndDate, value model.employeeHoursRange.end ] []
                    ]
                ]
            , div [ class "mdl-grid" ]
                [ div [ class "mdl-cell mdl-cell--3-col mdl-cell--6-col-phone" ]
                    [ select [ onInput SelectEmployee ] employeeOptions ]
                ]
            , div [ class "mdl-grid" ]
                [ div [ class "mdl-cell mdl-cell--2-col mdl-cell--6-col-phone" ]
                    (case ( url, filename, payload ) of
                        ( url, Just filename, Just payload ) ->
                            [ button [ onClick (DownloadEmployeeHoursFile filename payload) ] [ text "Hent rapport" ] ]

                        ( _, _ , _) ->
                            [ button [ disabled True ] [ text "Hent rapport" ] ]
                    )
                ]
            ]

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP

getEmployees : String -> String -> Cmd Msg
getEmployees token apiUrl =
    let
        url =
            apiUrl ++ "/employees"

        request =
            { method = "GET"
            , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
            , url = url
            , body = Http.emptyBody
            , expect = Http.expectJson decodeEmployee
            , timeout = Nothing
            , withCredentials = False
            }
    in Http.send SetEmployees <| Http.request request

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
        Http.send SetProjects <| Http.request request

decodeEmployee : Decoder (List Employee)
decodeEmployee =
    decode Employee
        |> required "first_name" string
        |> required "last_name" string
        |> required "id" int
        |> required "termination_date" (nullable string)
        |> list

decodeProject : Decoder (List Project)
decodeProject =
    decode Project
        |> required "projectId" string
        |> required "projectName" string
        |> required "customerName" string
        |> list
