import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import Navigation

import Json.Decode as D
import Json.Decode.Pipeline as P

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
  , alert : Maybe String
  , items : List Experiment
  , selectedItem : Maybe Experiment
  , connection : Maybe String
  , debugs : List Trace
  , candidates : Int
  , showCandidate : Maybe (Int,TS,Correctness)
  , resultTS : Maybe TS
  , loc : String
  }

prependBounded : Int -> a -> List a -> List a
prependBounded n a l = List.take n (a::l)

type Msg =
    ExpSelected (Dropdown.Msg Experiment)
  | ServerMsg SMsg
  | NoMsg
  | BadServerMsg

init : Navigation.Location -> (Model, Cmd Msg)
init l =
  (Model
    Dropdown.init
    Nothing
    [ Experiment "/static/asdf.png" "CountDownTimer"
    , Experiment "/static/asdf.png" "AsyncTask"
    , Experiment "/static/asdf.png" "SQLiteOpenHelper"
    ]
    Nothing
    Nothing
    []
    0
    Nothing
    Nothing
    l.hostname
  ,Cmd.none)

-- UPDATE

type SMsg = SAlert String
          | SQueryOk (List String) (List String)
          | SQueryNo (List String)
          | SCheck String
          | SCex (List String)
          | SResult String

decodeSMsg : D.Decoder SMsg
decodeSMsg =
  D.oneOf [ D.field "alert" decSAlert
          , D.field "queryOk" decqok
          , D.field "queryNo" decqno
          , D.field "check" deccheck
          , D.field "cex" deccex
          , D.field "result" decresult
          ]

decSAlert =
  P.decode SAlert
    |> P.required "message" D.string
decqok =
  P.decode SQueryOk
    |> P.required "inputs" (D.list D.string)
    |> P.required "outputs" (D.list D.string)
decqno =
  P.decode SQueryNo
    |> P.required "inputs" (D.list D.string)
deccheck =
  P.decode SCheck
    |> P.required "uri" D.string
decresult =
  P.decode SResult
    |> P.required "uri" D.string
deccex =
  P.decode SCex
    |> P.required "inputs" (D.list D.string)

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
                , debugs = []
                , candidates = 0
                , showCandidate = Nothing
                , resultTS = Nothing
            }
            , WebSocket.send (wsUrl model.loc) ("req:" ++ exp.name))
          _ -> ({ model | dropdown = updatedDropdown }, Cmd.none)
    ServerMsg m -> case m of
      SAlert t -> ({ model | alert = Just t }, Cmd.none)
      SQueryOk is os -> case (is,os) of
        ([],[]) -> (model,Cmd.none)
        _ -> ({ model | debugs = model.debugs ++ [(TestOk is os)] }, Cmd.none)
      SQueryNo is -> ({ model | debugs = model.debugs ++ [(TestErr is)] }, Cmd.none)
      SCex is -> ({ model | debugs = model.debugs ++ [(Cex model.candidates is)],
                            showCandidate = case model.showCandidate of
                                              Just (ts,uri,_) -> Just (ts,uri,Bad)
                                              Nothing -> Nothing }, Cmd.none)
      SCheck uri -> ({ model | debugs = model.debugs ++ [(Candidate (cnd model) uri)],
                               showCandidate = Just (cnd model,uri,Unsure),
                               candidates = cnd model}, Cmd.none)
      SResult uri -> ({ model | debugs = model.debugs ++ [(Finished (model.candidates) uri)],
                                resultTS = Just uri,
                                showCandidate = Nothing }, Cmd.none)
    BadServerMsg -> (model,Cmd.none)

cnd model = model.candidates + 1

type Trace = TestOk (List String) (List String)
           | TestErr (List String)
           | Cex Int (List String)
           | Candidate Int String
           | Finished Int String

-- SUBS

tryDecServerMsg s = case D.decodeString decodeSMsg s of
  Ok m -> ServerMsg m
  Err _ -> BadServerMsg

subscriptions : Model -> Sub Msg
subscriptions model = WebSocket.listen (wsUrl model.loc) tryDecServerMsg

-- VIEW

resultsHost = "127.0.0.1"

resultsURI : String -> String -> String
resultsURI l f = "http://" ++ l ++ ":30025/" ++ f

renderTrace : Trace -> Html msg
renderTrace t = case t of
  TestOk is os ->
    span
      [ class "trace" ]
      ([ span [] (List.map (\s -> span [class "okin"] [text s]) is) ]
       ++ case os of
            [] -> []
            os -> [ span [ class "tracesep" ] [text ">>"]
                  , span [] (List.map (\s -> span [class "okout"] [text s]) os) ])
  TestErr is ->
    span
      [ class "trace" ]
      [span [] (List.map (\s -> span [class "errin"] [text s]) is)]
  Candidate n _ -> text ("Checking candidate #" ++ toString n ++ "...")
  Finished n _ -> text ("Finished: Candidate #" ++ toString n ++ " is correct.")
  Cex n is ->
    span
      [class "cex"]
      [ span [] [text ("Counter-example found for #" ++ toString n ++ ":")]
      , span [] (List.map (\s -> span [class "cexin"] [text s]) is) ]

type Correctness = Good | Unsure | Bad

tsImg : Model -> TS -> Correctness -> Html msg
tsImg model ts cor =
  let cls = case cor of
              Good -> "good-ts"
              Unsure -> "unsure-ts"
              Bad -> "bad-ts"
  in img [ class cls, src (resultsURI model.loc ts) ] []

view : Model -> Html Msg
view model =
  div []
    [ Html.node "link" [ Html.Attributes.rel "stylesheet"
                       , Html.Attributes.href "/css/dropdown.css" ] []
    , h1 [] [text "Inputs"]
    , Html.map ExpSelected <|
        Dropdown.view
          model.items
          model.selectedItem
          .name
          model.dropdown
    , h1 [] [text "Learning"]
    , div []
      [ ul
          [class "traces"]
          (List.map
             (\t -> li [] [renderTrace t])
             (List.reverse (model.debugs)))
      ]
    , div [] (case model.showCandidate of
               Just (num,ts,Unsure) -> [ h2 [] [text ("Checking candidate #" ++ (toString num) ++ ":")]
                                       , tsImg model ts Unsure ]
               Just (num,ts,Bad) -> [ h2 [] [text ("Eliminated candidate #" ++ (toString num) ++ ".  Building next...")]
                                    , tsImg model ts Bad ]
               Just (num,ts,Good) -> [tsImg model ts Good]
               Nothing -> [])
    , h1 [] [text "Results"]
    , case model.resultTS of
        Just ts -> tsImg model ts Good
        Nothing -> text ""
    ]
