import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import Navigation

import Dropdown exposing (Dropdown, Event(ItemSelected))

main =
  Navigation.program
    (\_ -> NoMsg)
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
  , loc : String
  }

prependBounded : Int -> a -> List a -> List a
prependBounded n a l = List.take n (a::l)

type Msg = ExpSelected (Dropdown.Msg Experiment) | Connected String | NoMsg

init : Navigation.Location -> (Model, Cmd Msg)
init l =
  (Model
    Dropdown.init
    [ Experiment "/static/asdf.png" "CountDownTimer"
    , Experiment "/static/asdf.png" "AsyncTask"
    ]
    Nothing
    Nothing
    []
    Nothing
    l.hostname
  ,Cmd.none)

-- UPDATE

wsUrl : String -> String
wsUrl hostname = "ws://" ++ hostname ++ ":30025"
-- wsUrl _ = "ws://" ++ "127.0.0.1" ++ ":30025"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoMsg -> (model, Cmd.none)
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
            , WebSocket.send (wsUrl model.loc) ("req:" ++ exp.name))
          _ -> ({ model | dropdown = updatedDropdown }, Cmd.none)
    -- Connected s -> ({ model | connection = Just s }, Cmd.none)
    Connected s ->
      -- let k = String.split ":" s
      let k = String.left 4 s
          p = String.dropLeft 4 s
      in case k of
        ("dbg:") -> ({ model | debugs = prependBounded 10 p model.debugs }, Cmd.none)
        ("res:") -> ({ model | resultTS = Just p }, Cmd.none)
        _ -> ({ model | connection = Just s }, Cmd.none)


-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model = WebSocket.listen (wsUrl model.loc) Connected

-- VIEW

resultsHost = "127.0.0.1"

resultsURI : String -> String -> String
resultsURI l f = "http://" ++ l ++ ":30025/" ++ f

view : Model -> Html Msg
view model =
  div []
    [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "/css/dropdown.css" ] []
    , h1 [] [text "Inputs"]
    , Html.map ExpSelected <|
        Dropdown.view
          model.items
          model.selectedItem
          .name
          model.dropdown
    , h1 [] [text "Learning"]
    , div []
      [ div [] (List.map (\dm -> pre [] [text dm]) (List.reverse model.debugs))
      ]
    , h1 [] [text "Results"]
    , case model.resultTS of
        Just ts -> img [ src (resultsURI model.loc ts) ] []
        Nothing -> text ""
    ]
