import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing (onClick, on, targetValue)
import Http
import Json.Decode as Json
import Json.Decode exposing ((:=))
import String
import List
import Task
import Date exposing (..)

apiUrl : String
apiUrl = "https://api.floq.no/reporting/"

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

intDecoder : Json.Decoder Int
intDecoder =
  targetValue `Json.andThen` \val ->
    case String.toInt val of
      Ok i -> Json.succeed i
      Err err -> Json.fail err

main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type alias Project =
  { id : Int
  , name : String
  , customer: String
  }

type alias Model =
  { projects : List Project
  , year : Int
  , month : Int
  }

init : (Model, Cmd Msg)
init = (Model [] 1970 1, Task.perform Initialize Initialize Date.now)

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
        ({ model | year = Date.year date, month = monthToInt (Date.month date) }, getProjects)

    FetchSucceed projects ->
      (Model projects model.year model.month, Cmd.none)

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
  let items = (List.map (\p -> li [] [a [href (apiUrl ++ "hours/" ++ toString p.id ++ "?year=" ++ toString model.year ++ "&month=" ++ toString model.month)] [text (p.customer ++ ": " ++ p.name)]]) model.projects)
      monthOptions = List.map
                       (\m -> option
                            [selected (monthToInt m == model.month), value (toString (monthToInt m))]
                            [text (toString m)])
                       months
      yearOptions = List.map
                      (\m -> option
                           [selected (m == model.year), value (toString m)]
                           [text (toString m)])
                      [2015..2025]
  in
  div []
    [ h2 [] [text "Projects"]
    , select [on "change" (Json.map MonthChanged intDecoder)] monthOptions
    , select [on "change" (Json.map YearChanged intDecoder)] yearOptions
    , br [] []
    , ul [] items
    ]



-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- HTTP
getProjects : Cmd Msg
getProjects =
  let url = apiUrl ++ "projects"
  in Task.perform FetchFail FetchSucceed (Http.get decodeProject url)

decodeProject : Json.Decoder (List Project)
decodeProject =
  Json.list (Json.object3 Project ("projectId" := Json.int) ("projectName" := Json.string) ("customerName" := Json.string))
