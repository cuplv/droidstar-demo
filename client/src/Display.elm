module Display exposing (..)

import Types exposing (..)
import Exp exposing (..)
import ServerComm exposing (tsURI)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dropdown exposing (Dropdown, Event(ItemSelected))

view : Model -> Html Msg
view model =
  div [] <|
    [ styleHeader
    , inputSection model
    ] ++
    (case model.selectedItem of
       Just e ->
         [ learnSection model.netConf e
         , resultsSection model.netConf e
         ]
       Nothing -> [])

styleHeader : Html msg
styleHeader = node "link" [ rel "stylesheet", href "/css/dropdown.css" ] []

lp2Text : LP -> Html msg
lp2Text lp = text lp.lpText

lpInput : Exp -> Html Msg
lpInput e = case e.status of
  Editing -> div []
    [ div [] [textarea [spellcheck False, onInput UpdateLP] [lp2Text e.lp]]
    , div [] [button [onClick BeginLearn] [text "Learn"]]
    ]
  _ -> div []
    [ textarea [spellcheck False, readonly True] [lp2Text e.lp]
    ]

inputSection : Model -> Html Msg
inputSection model = div [] <|
  [ h1 [] [text "Inputs"]
  , Html.map ExpSelected <|
      Dropdown.view
        model.items
        (case model.selectedItem of
           Just e -> Just e.lp
           Nothing -> Nothing)
        .name
        model.dropdown
  ] ++
  (case model.selectedItem of
     Just e -> [lpInput e]
     Nothing -> [])


trace : Bool -> LearnTrace -> Html Msg
trace finished t = case t of
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
  Check n ts -> case finished of
    False -> span [class "cand"] [text ("Checking candidate #" ++ toString n ++ "...")]
    True ->
      span
        [class "candf"]
        [ text "Checking"
        , button [ onClick (ReviewCandidate n ts) ] [ text ("candidate #" ++ toString n) ]
        , text "..." ]
  Confirmed n -> text ("Finished: Candidate #" ++ toString n ++ " is correct.")
  Cex n is ->
    span
      [class "cex"]
      [ span [] [text ("Counter-example found for #" ++ toString n ++ ":")]
      , span [] (List.map (\s -> span [class "cexin"] [text s]) is) ]

traces : List LearnTrace -> Html Msg
traces ls = ul 
  [class "traces"] 
  (List.map
     (\t -> li [] [trace (confirmed ls) t])
     (List.reverse ls))

learnStatus : NetConf -> List LearnTrace -> List (Html msg)
learnStatus nc ls = if confirmed ls then [] else
  let (s,mts) = traceInfo ls in
  [ h2 [] [text s] ]
  ++
  (case mts of
    Just ts -> [showTS nc ts]
    Nothing -> [])

showTS : NetConf -> ShowTS -> Html msg
showTS nc (ShowTS ts cor) = 
  let c = case cor of
            Good -> "good-ts"
            Unsure -> "unsure-ts"
            Bad -> "bad-ts"
  in img [ class c, src (tsURI nc ts) ] []

learnSection : NetConf -> Exp -> Html Msg
learnSection nc e = case e.status of

  Running ls -> div [] <|
    [ h1 [] [text "Learning"]
    , traces ls
    ]
    ++
    learnStatus nc ls

  Finished ls mts -> div [] <|
    [ h1 [] [text "Learning"]
    , traces ls
    ]
    ++
    (case mts of
       Just ts -> [showTS nc ts]
       Nothing -> [])

  _ -> div [] []

resultsSection : NetConf -> Exp -> Html msg
resultsSection nc e = case resultTS e of
  Just ts -> div [] 
    [ h1 [] [text "Results"]
    , showTS nc ts
    ]
  _ -> div [] []
