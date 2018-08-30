module Display exposing (..)

import Types exposing (..)
import Exp exposing (..)
import ServerComm exposing (tsURI)
import BigContent exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
-- import Dropdown exposing (Dropdown, Event(ItemSelected))

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Button as Button

view : Model -> Html Msg
view model = Grid.container []
  [ CDN.stylesheet
  , navbar model
  , view2 model
  ]

navbar : Model -> Html Msg
navbar model =
  Navbar.config (\_ -> NoMsg)
    -- |> Navbar.withAnimation
    |> Navbar.container
    |> Navbar.brand [ href "#" ] [ text "DroidStar" ]
    |> Navbar.items
       [ Navbar.itemLink
           [ href "http://plv.colorado.edu/droidstar/demo-tutorial" ]
           [ text "Tutorial" ]
       ]
    |> Navbar.view (Tuple.first (Navbar.initialState (\_ -> NoMsg)))

view2 : Model -> Html Msg
view2 model = case model.netConf.connection of
  Just mode ->
    div [] <|
      [ styleHeader
      -- , alertSection model.alertLog
      , inputSection model mode
      ] ++
      (case model.selectedItem of
         Just e ->
           [ learnSection model.netConf e
           , resultsSection model.netConf e
           ]
         Nothing -> [])
  Nothing -> div [] [text "Waiting for available server..."]

alertSection : List Alert -> Html msg
alertSection ls = case ls of
  [] -> div [] []
  a::ls -> div [] [text a]

styleHeader : Html msg
styleHeader = node "link" [ rel "stylesheet", href "/css/dropdown.css" ] []

lp2Text : LP -> Html msg
lp2Text lp = text lp.lpText

lpInput : Exp -> ServerMode -> Html Msg
lpInput e mode = case (e.status,mode) of
  (Editing Nothing,Custom) -> div []
    [ div [] [textarea [spellcheck False, onInput UpdateLP] [lp2Text e.lp]]
    , div [] [button [onClick BeginLearn] [text "Learn"]]
    ]
  (Editing (Just _),Custom) -> div []
    [ div [] [textarea
                [spellcheck False, onInput UpdateLP, class "compileFailed"]
                [lp2Text e.lp]]
    , div [] [button [onClick BeginLearn] [text "Learn"]]
    ]
  (Editing _,Static) -> div []
    [ textarea [spellcheck False, readonly True] [lp2Text e.lp]
    , div [] [button [onClick BeginLearn] [text "Learn"]]
    ]
  (Compiling, Custom) -> div []
    [ textarea [spellcheck False, readonly True, class "compiling"] [lp2Text e.lp]
    ]
  (_, Custom) -> div []
    [ textarea [spellcheck False, readonly True, class "compileSucceeded"] [lp2Text e.lp]
    ]
  _ -> div []
    [ textarea [spellcheck False, readonly True] [lp2Text e.lp]
    ]

inputSection : Model -> ServerMode -> Html Msg
inputSection model mode = div [] <|
  -- [ h1 [] [text "Inputs"]
  (case model.selectedItem of 
     Just _ -> []
     Nothing -> [ inputsDoc ])
  ++
  [ Dropdown.dropdown
      model.dropdown
      { options = [ ]
      , toggleMsg = DropdownUpdate
      , toggleButton =
          Dropdown.toggle
            [ Button.primary ]
            [ text (case model.selectedItem of
                      Just exp -> exp.lp.name
                      Nothing -> "Classes") ]
      , items = List.map (\lp ->
          Dropdown.buttonItem [ onClick (ExpSelected lp) ] [ text lp.name ]
        ) model.items
      }
  -- , Html.map ExpSelected <|
  --     Dropdown.view
  --       model.items
  --       (case model.selectedItem of
  --          Just e -> Just e.lp
  --          Nothing -> Nothing)
  --       .name
  --       model.dropdown
  ]
  ++
  (case model.selectedItem of
   Just e -> [e.lp.docs]
   Nothing -> [])
  ++
  (case model.selectedItem of
     Just e -> [lpInput e mode]
     Nothing -> [])

numberInputs : List String -> List String
numberInputs = numi 1

numi : Int -> List String -> List String
numi n ls = case ls of
  "(CB?)" :: ls2 ->
    ("(CB " ++ toString n ++ "?)") :: numi (n + 1) ls2
  l :: ls2 -> l :: numi n ls2
  [] -> []

numberOutputs : List String -> List String
numberOutputs = numo 1

numo : Int -> List String -> List String
numo n ls = case ls of
  l :: ls2 -> (toString n ++ ": " ++ l) :: numo (n + 1) ls2
  [] -> []

trace : Bool -> LearnTrace -> Html Msg
trace finished t = case t of
  TestOk is os ->
    span
      [ class "trace" ]
      ([ span [] (List.map (\s -> span [class "okin"] [text s]) (numberInputs is)) ]
       ++ case os of
            [] -> []
            os -> [ span [ class "tracesep" ] [text ">>"]
                  , span [] (List.map (\s -> span [class "okout"] [text s]) (numberOutputs os)) ])
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

  Editing (Just s) -> div [] <|
    [ h1 [] [text "Learning"]
    , div [] [text "Compiler error:"]
    , div [] [textarea [spellcheck False, readonly True] [text s]]
    ]

  Compiling -> div [] <|
    [ h1 [] [text "Learning"]
    , div [] [text "Compiling custom LearningPurpose..."]
    , img [ class "loadingicon", src "/static/triangles.gif" ] []
    ]

  Running ls -> div [] <|
    [ h1 [] [text "Learning"]
    , learningDoc
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
    , resultsDoc
    , showTS nc ts
    ]
  _ -> div [] []
