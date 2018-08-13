module Types exposing (..)

import Dropdown exposing (Dropdown)

type alias LP =
  { lpText : String
  , name : String
  }

editLP : String -> LP -> LP
editLP s lp = { lp | lpText = s }

type Correctness = Good | Unsure | Bad

type TS = TS String

type ShowTS = ShowTS TS Correctness

initSTS : TS -> ShowTS
initSTS ts = ShowTS ts Unsure

refuteSTS : ShowTS -> ShowTS
refuteSTS (ShowTS ts _) = ShowTS ts Bad

confirmSTS : ShowTS -> ShowTS
confirmSTS (ShowTS ts _) = ShowTS ts Good

type alias Alert = String

type LearnTrace = 
    TestOk (List String) (List String)
  | TestErr (List String)
  | Check Int TS
  | Cex Int (List String)
  | Confirmed Int

type alias Exp =
  { lp : LP
  , status : ExpStatus
  }

type ExpStatus = 
    Editing
  | Compiling
  | Running (List LearnTrace)
  | Finished (List LearnTrace) (Maybe ShowTS)

type alias Model =
  { dropdown : Dropdown
  , alertLog : List Alert
  , items : List LP
  , selectedItem : Maybe Exp
  , netConf : NetConf
  }

compiling : Model -> Model
compiling m = case m.selectedItem of
  Just e -> { m | selectedItem = Just { e | status = Compiling } }
  _ -> m

-- updateLP : String -> Model -> Model
-- updateLP s model = case model.selectedItem of
--   Just e -> case e.status of
--     Editing -> { model | selectedItem = Just { e | lp = editLP s e.lp } }
--     _ -> model
--   _ -> model

skip : Model -> (Model, Cmd Msg)
skip m = (m,Cmd.none)

onExpM : Model -> (Exp -> (Model, Cmd Msg)) -> (Model, Cmd Msg)
onExpM m f = case m.selectedItem of
  Just e -> f e
  Nothing -> skip m

onExp : Model -> (Exp -> (Exp, Cmd Msg)) -> (Model, Cmd Msg)
onExp m f = case m.selectedItem of
  Just e -> case f e of
    (e,c) -> ({m | selectedItem = Just e}, c)
  Nothing -> skip m

onExpRun : Model -> (List LearnTrace -> (ExpStatus, Cmd Msg)) -> (Model, Cmd Msg)
onExpRun m f = onExp m (\e -> case e.status of
  Running ts -> case f ts of
    (es,c) -> ({ e | status = es }, c)
  _ -> (e,Cmd.none))

addTrace : Model -> LearnTrace -> (Model, Cmd Msg)
addTrace m t = onExpRun m (\ts -> (Running (ts ++ [t]), Cmd.none))

onExpEdit : Model -> (LP -> (Exp, Cmd Msg)) -> (Model, Cmd Msg)
onExpEdit m f = onExp m (\e -> case e.status of
  Editing -> f e.lp
  _ -> (e,Cmd.none))

onExpFin : Model
         -> ((List LearnTrace, Maybe ShowTS) -> (Maybe ShowTS, Cmd Msg))
         -> (Model, Cmd Msg)
onExpFin m f = onExp m (\e -> case e.status of
  Finished ls msts -> case f (ls,msts) of
    (msts2,c) -> ({ e | status = Finished ls msts2 }, c)
  _ -> (e,Cmd.none))

updateLP : String -> LP -> Exp
updateLP s lp =
  { lp = { lpText = s
         , name = lp.name
         }
  , status = Editing
  }

type Msg =
    ExpSelected (Dropdown.Msg LP)
  | ServerMsg ServerMsg
  | NoMsg
  | BadServerMsg String
  | ReviewCandidate Int TS
  | UpdateLP String
  | BeginLearn

type ServerMsg =
    SAlert String
  | SCompiled
  | STrace STrace

type STrace = 
    SQueryOk (List String) (List String)
  | SQueryNo (List String)
  | SCheck String
  | SCex (List String)
  | SResult String

type alias NetConf =
  { loc : String
  , connection : Maybe String
  }
