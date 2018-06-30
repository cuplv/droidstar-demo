import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

import Dropdown exposing (Dropdown, Event(ItemSelected))

main =
  Html.beginnerProgram
    { model = init
    , view = view
    , update = update
    }

type alias Experiment =
  { imgSrc : String
  , name : String
  }

-- MODEL


type alias Model =
  { dropdown : Dropdown
  , items : List Experiment
  , selectedItem : Maybe Experiment
  }

type Msg = ExpSelected (Dropdown.Msg Experiment)

init =
  Model
    Dropdown.init
    [ Experiment "/static/async.png" "AsyncTask"
    , Experiment "/static/bluetooth.png" "BluetoothAdapter"
    ]
    Nothing

-- UPDATE


update : Msg -> Model -> Model
update msg model =
  case msg of
    ExpSelected ddmsg ->
      let
        ( updatedDropdown, event ) =
          Dropdown.update ddmsg model.dropdown
      in
        case event of
          ItemSelected exp ->
            { model
                | dropdown = updatedDropdown
                , selectedItem = Just exp
            }
          _ -> { model | dropdown = updatedDropdown }

imgsrc : Model -> String
imgsrc model =
  case model.selectedItem of
    Nothing -> "/static/nothing.png"
    Just e -> e.imgSrc

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
    , Html.img [ src (imgsrc model), width 300, height 300] []
    ]
