module Exp exposing (..)

import Types exposing (..)



traceInfo : List LearnTrace -> (String, Maybe ShowTS)
traceInfo lts =
  let f : LearnTrace -> (String,Maybe ShowTS) -> (String,Maybe ShowTS)
      f t (s,sts) = case t of
        Check n ts -> ("Checking #" ++ toString n, Just (initSTS ts))
        Cex n _ -> ("Refuted #" ++ toString n, Maybe.map refuteSTS sts)
        Confirmed n -> ("Candidate #" ++ toString n ++ " confirmed.", Nothing)
        _ -> (s,sts)
  in List.foldl f ("Building first candidate...",Nothing) lts
