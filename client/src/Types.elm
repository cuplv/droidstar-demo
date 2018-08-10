module Types exposing (..)

import Dropdown exposing (Dropdown)

type alias LP =
  { lpText : String
  , name : String
  }

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
  , connection : Maybe String
  , loc : String
  }

type Msg =
    ExpSelected (Dropdown.Msg LP)
  | ServerMsg ServerMsg
  | NoMsg
  | BadServerMsg
  | ReviewCandidate (ShowTS)
  | UpdateLP String
  | BeginLearn

type ServerMsg =
    SAlert String
  | SQueryOk (List String) (List String)
  | SQueryNo (List String)
  | SCheck String
  | SCex (List String)
  | SResult String
