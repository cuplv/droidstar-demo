import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import Navigation
import Json.Decode as D
import Json.Decode.Pipeline as P
import Dropdown exposing (Dropdown, Event(ItemSelected))
import Markdown

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
  | ShowC Int TS

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
    ShowC n uri -> case model.resultTS of
      Just finalTS ->
        let status = if finalTS == uri then Good else Bad
        in ({ model | showCandidate = Just (n,uri,status) }, Cmd.none)
      Nothing -> (model, Cmd.none)

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

renderTrace : Bool -> Trace -> Html Msg
renderTrace finished t = case t of
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
  Candidate n uri -> case finished of
    False -> span [class "cand"] [text ("Checking candidate #" ++ toString n ++ "...")]
    True ->
      span
        [class "candf"]
        [ text "Checking"
        , button [ onClick (ShowC n uri) ] [ text ("candidate #" ++ toString n) ]
        , text "..." ]
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

checkFin : Model -> Bool
checkFin model = case model.resultTS of
  Just _ -> True
  Nothing -> False

inputsDoc = Markdown.toHtml [ class "docs" ] """

Begin by clicking one of the class names below.

"""

learningDoc = Markdown.toHtml [ class "docs" ] """

DroidStar learns by performing **queries**, which are sequences of
callins interspersed with pauses to listen for callbacks.

These pauses are written in the following logs as <span
class="okin">(CB?)</span> blocks.  Each pause gets a blue response
block after the ">>", which either contains a callback or a <span
class="okout">(none)</span> if none were seen.

Queries are **accepted** if running the callins in sequence does not
throw an error.  In this case they will appear green and followed by
(blue) callbacks.  If one of the callins throws an error, the query is
**rejected** and appears red in the log.

"""

resultsDoc = Markdown.toHtml [ class "docs" ] """

The result of a learning session is a **callback typestate**,
presented below as a graph, which describes a class's stateful behavior.

A class object begins in state 0, and allows only those callins which
have arrows leaving state 0 to be called.  Upon having an accepted
callin invoked, the object changes to the state indicated by the
callin's arrow.

Somes states have a callback enabled, marked by an arrow labeled with
`cb_something`.  This indicates that if you leave the object in this
state, you will observe that callback.

Callback typestates are built using query results.  Try looking back
through the query log that produced this result, and comparing queries
to the graph.  For <span class="okin">accepted</span> queries, you
will be able to create a path from the callins and callbacks in the
graph (where <span class="okout">(none)</span> makes no move).  For
<span class="errin">rejected</span> queries, you will find that one
callin in the sequence is applied in a state where it is not enabled
(there is no arrow labeled with it leaving the state).

"""

view : Model -> Html Msg
view model =
  div []
    [ Html.node "link" [ Html.Attributes.rel "stylesheet"
                       , Html.Attributes.href "/css/dropdown.css" ] []
    , h1 [] [text "Inputs"]
    , inputsDoc
    , Html.map ExpSelected <|
        Dropdown.view
          model.items
          model.selectedItem
          .name
          model.dropdown
    , h1 [] [text "Learning"]
    , case model.selectedItem of
        Just _ -> learningDoc
        Nothing -> text ""
    , div []
      [ ul
          [class "traces"]
          (List.map
             (\t -> li [] [renderTrace (checkFin model) t])
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
        Just _ -> resultsDoc
        Nothing -> text ""
    , case model.resultTS of
        Just ts -> tsImg model ts Good
        Nothing -> text ""
    ]
