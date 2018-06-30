import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket

import Dropdown exposing (Dropdown, Event(ItemSelected))

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Experiment =
  { imgSrc : String
  , name : String
  }

-- MODEL

type alias TS = String

type alias Model =
  { dropdown : Dropdown
  , items : List Experiment
  , selectedItem : Maybe Experiment
  , connection : Maybe String
  , debugs : List String
  , resultTS : Maybe TS
  }

prependBounded : Int -> a -> List a -> List a
prependBounded n a l = List.take n (a::l)

type Msg = ExpSelected (Dropdown.Msg Experiment) | Connected String

init : (Model, Cmd Msg)
init =
  (Model
    Dropdown.init
    [ Experiment "/static/asdf.png" "CountDownTimer"
    , Experiment "/static/asdf.png" "AsyncTask"
    ]
    Nothing
    Nothing
    []
    Nothing
  ,Cmd.none)

-- UPDATE

wsUrl : String
wsUrl = "ws://localhost:3000"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ExpSelected ddmsg ->
      let
        ( updatedDropdown, event ) =
          Dropdown.update ddmsg model.dropdown
      in
        case event of
          ItemSelected exp ->
            ({ model
                | dropdown = updatedDropdown
                , selectedItem = Just exp
            }
            , WebSocket.send wsUrl ("req:" ++ exp.name))
          _ -> ({ model | dropdown = updatedDropdown }, Cmd.none)
    -- Connected s -> ({ model | connection = Just s }, Cmd.none)
    Connected s ->
      let k = String.split ":" s
      in case k of
        ("dbg"::m) -> ({ model | debugs = prependBounded 10 (String.concat m) model.debugs }, Cmd.none)
        ("rsl"::m) -> ({ model | resultTS = Just (String.concat m) }, Cmd.none)
        _ -> ({ model | connection = Just s }, Cmd.none)

imgsrc : Model -> String
imgsrc model =
  case model.selectedItem of
    Nothing -> "/static/nothing.png"
    Just e -> e.imgSrc

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model = WebSocket.listen wsUrl Connected

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "/css/dropdown.css" ] []
    , Html.map ExpSelected <|
        Dropdown.view
          model.items
          model.selectedItem
          .name
          model.dropdown
    -- , Html.img [ src (imgsrc model), width 300, height 300] []
    , div []
      [ div [] (List.map (\dm -> pre [] [text dm]) (List.reverse model.debugs))
      , case model.resultTS of
          Just ts -> pre [] [text ts]
          Nothing -> text ""
      ]
    ]
