module Exp exposing (..)

import Types exposing (..)

{-| Get description and possible TS to show for a trace log -}
traceInfo : List LearnTrace -> (String, Maybe ShowTS)
traceInfo lts =
  let f : LearnTrace -> (String,Maybe ShowTS) -> (String,Maybe ShowTS)
      f t (s,sts) = case t of
        Check n ts -> ("Checking #" ++ toString n, Just (initSTS ts))
        Cex n _ -> ("Refuted #" ++ toString n, Maybe.map refuteSTS sts)
        Confirmed n -> ("Candidate #" ++ toString n ++ " confirmed.", Nothing)
        _ -> (s,sts)
  in List.foldl f ("Building first candidate...",Nothing) lts

{-| Count candidates proposed in a trace log -}
candCount : List LearnTrace -> Int
candCount ls = case ls of
  Check _ _ :: ls -> 1 + candCount ls
  _         :: ls -> 0 + candCount ls
  _               -> 0

{-| Update a trace log with a trace from the server -}
handleSTrace : STrace -> List LearnTrace -> List LearnTrace
handleSTrace t ls = case t of
  SQueryOk ins outs -> ls ++ [TestOk ins outs]
  SQueryNo ins      -> ls ++ [TestErr ins]
  SCheck uri        -> ls ++ [Check (candCount ls + 1) (TS uri)]
  SCex ins          -> ls ++ [Cex (candCount ls) ins]
  SResult _         -> ls ++ [Confirmed (candCount ls)]

{-| Check if a trace log is complete -}
confirmed : List LearnTrace -> Bool
confirmed ls = case ls of
  Confirmed _ :: _ -> True
  l :: ls -> confirmed ls
  [] -> False

resultTS : Exp -> Maybe ShowTS
resultTS e = case e.status of
  Finished ls _ -> case List.foldl lastTS Nothing ls of
    Just ts -> Just (ShowTS ts Good)
    _ -> Nothing
  _ -> Nothing

lastTS : LearnTrace -> Maybe TS -> Maybe TS
lastTS l mt = case l of
  Check _ ts -> Just ts
  _ -> mt
