import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import Navigation
import Dropdown exposing (Dropdown, Event(ItemSelected))
import Markdown

import BigContent exposing (..)
import Display exposing (view)
import Exp exposing (..)
import ServerComm exposing (..)
import Types exposing (..)


main = 
  Navigation.program
    (\_ -> NoMsg)
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : Navigation.Location -> (Model, Cmd Msg)
init l = 
  (Model
     Dropdown.init
     []
     [ { name = "AsyncTask", lpText = asyncTaskDef }
     ]
     Nothing
     { loc = l.hostname, connection = Nothing }
  ,Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let skip = (model, Cmd.none) in
  case msg of

    NoMsg -> skip

    ExpSelected ddmsg ->
      let
        ( updatedDropdown, event ) =
          Dropdown.update ddmsg model.dropdown
      in
        case event of
          ItemSelected exp ->
            ({ model
                | dropdown = updatedDropdown
                , selectedItem = Just { lp = exp, status = Editing }
                , alertLog = []
            }
            , Cmd.none)
          _ -> ({ model | dropdown = updatedDropdown }, Cmd.none)

    ServerMsg smsg -> case smsg of
      SHello m ->
        ({ model | netConf =
             { loc = model.netConf.loc, connection = Just m } }, Cmd.none)
      SAlert s -> ({ model | alertLog = s :: model.alertLog }, Cmd.none)
      SCompiled -> onExp model (\e -> case e.status of
        Compiling -> ({ e | status = Running [] }, Cmd.none)
        _ -> (e, Cmd.none))
      STrace t -> case t of
        SQueryOk is os -> case (is,os) of
          ([],[]) -> skip
          _ -> addTrace model (TestOk is os)
        SQueryNo is -> addTrace model (TestErr is)
        SCex is ->
          onExpRun model (\ts ->
            (Running (ts ++ [(Cex (candCount ts) is)]), Cmd.none))
        SCheck uri ->
          onExpRun model (\ts ->
            (Running (ts ++ [(Check (candCount ts + 1) (TS uri))]), Cmd.none))
        SResult _ ->
          onExpRun model (\ts ->
            (Finished (ts ++ [(Confirmed (candCount ts))]) Nothing, Cmd.none))

    BadServerMsg s ->
      ({ model | alertLog = ("Bad server msg: " ++ s) :: model.alertLog } , Cmd.none)

    ReviewCandidate i ts -> onExpFin model (\(ls,_) ->
      if i == candCount ls
         then (Just (ShowTS ts Good), Cmd.none)
         else (Just (ShowTS ts Bad), Cmd.none))

    UpdateLP s -> onExpEdit model (\lp -> (updateLP s lp, Cmd.none))

    BeginLearn -> onExpEdit model (\lp -> 
      ( { lp = lp, status = Compiling }
      , serverReq model.netConf lp
      ))
