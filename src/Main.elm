import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing (onClick, on, targetValue)
import Http
import Json.Decode as Json
import Json.Decode exposing ((:=))
import String
import Char exposing (isLower, isUpper)
import List
import Task
import Date exposing (..)

months : List Month
months = [Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec]

monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan -> 1
        Feb -> 2
        Mar -> 3
        Apr -> 4
        May -> 5
        Jun -> 6
        Jul -> 7
        Aug -> 8
        Sep -> 9
        Oct -> 10
        Nov -> 11
        Dec -> 12

monthToString : Month -> String
monthToString month =
    case month of
        Jan -> "Januar"
        Feb -> "Februar"
        Mar -> "Mars"
        Apr -> "April"
        May -> "Mai"
        Jun -> "Juni"
        Jul -> "Juli"
        Aug -> "August"
        Sep -> "September"
        Oct -> "Okober"
        Nov -> "November"
        Dec -> "Desember"

intDecoder : Json.Decoder Int
intDecoder =
  targetValue `Json.andThen` \val ->
    case String.toInt val of
      Ok i -> Json.succeed i
      Err err -> Json.fail err

type alias Flags = { token : String, apiUrl : String }

main : Program Flags
main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type alias Project =
  { id : String
  , name : String
  , customer: String
  }

type alias Model =
  { projects : List Project
  , year : Int
  , month : Int
  , token : String
  , apiUrl : String
  }

init : Flags -> (Model, Cmd Msg)
init flags =
    let initialModel =  Model [] 1970 1 flags.token flags.apiUrl
    in
        (initialModel, Task.perform Initialize Initialize Date.now)

-- UPDATE
type Msg
  = FetchSucceed (List Project)
  | FetchFail Http.Error
  | Initialize Date.Date
  | YearChanged Int
  | MonthChanged Int
  | DateMissing


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Initialize date ->
        ({ model |
               year = Date.year date,
               month = monthToInt (Date.month date)
         }, getProjects model.token model.apiUrl)

    FetchSucceed projects ->
      ({ model | projects = projects }, Cmd.none)

    FetchFail _ ->
      (model, Cmd.none)

    YearChanged year ->
        ({ model | year = year }, Cmd.none)

    MonthChanged month ->
        ({ model | month = month }, Cmd.none)

    DateMissing ->
        (model, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model =
  let toListItem p =
          let url = model.apiUrl
                    ++ "/reporting/hours/" ++ p.id
                    ++ "?year=" ++ toString model.year
                    ++ "&month=" ++ toString model.month
                    ++ "&jwt=" ++ Http.uriDecode model.token
              month = if model.month < 10
                      then "0" ++ toString model.month
                      else toString model.month
              filename = toString model.year ++ "-"
                         ++ month ++ "-"
                         ++ String.filter (\c -> isUpper c || isLower c) p.name
                         ++ ".csv"
          in
          li
            [class "mdl-list__item"]
            [a
              [href url]
              [span [class "code"] [text p.id], text (": " ++ p.customer ++ " – " ++ p.name)]]
      items = (List.map toListItem model.projects)
      toMonthOption m = option
                     [selected (monthToInt m == model.month), value (toString (monthToInt m))]
                     [text (monthToString m)]
      monthOptions = List.map toMonthOption months
      toYearOption y = option
                         [selected (y == model.year), value (toString y)]
                         [text (toString y)]
      yearOptions = List.map toYearOption [2015..2025]
  in
  div []
    [ h3 [] [text "Prosjekter"]
    , div [class "mdl-grid"]
      [ div [class "mdl-cell mdl-cell--2-col mdl-cell--6-col-phone"]
        [ select [on "change" (Json.map MonthChanged intDecoder)] monthOptions
        ]
      , div [class "mdl-cell mdl-cell--2-col mdl-cell--6-col-phone"]
        [ select [on "change" (Json.map YearChanged intDecoder)] yearOptions
        ]
      ]
    , div []
      [ ul [class "mdl-list"] items]
    ]



-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- HTTP
getProjects : String -> String -> Cmd Msg
getProjects token apiUrl =
  let url = apiUrl ++ "/reporting/projects"
      request =
        { verb = "GET"
        , headers = [("Authorization", "Bearer " ++ token)]
        , url = url
        , body = Http.empty
        }
  in Task.perform FetchFail FetchSucceed (Http.fromJson decodeProject (Http.send Http.defaultSettings request))

decodeProject : Json.Decoder (List Project)
decodeProject =
  Json.list (Json.object3 Project ("projectId" := Json.string) ("projectName" := Json.string) ("customerName" := Json.string))
