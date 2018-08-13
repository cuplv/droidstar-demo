module ServerComm exposing (..)

import Types exposing (..)

import WebSocket
import Navigation
import Json.Decode as D
import Json.Decode.Pipeline as P
import Json.Encode exposing (encode,string)

serverPort : Int
serverPort = 30025

tsURI : NetConf -> TS -> String
tsURI nc (TS f) =
  "http://" ++ nc.loc
  ++ ":" ++ toString serverPort
  ++ "/" ++ f

decodeSMsg : D.Decoder ServerMsg
decodeSMsg =
  D.oneOf [ D.field "alert" decSAlert
          , D.field "queryOk" decqok
          , D.field "queryNo" decqno
          , D.field "check" deccheck
          , D.field "cex" deccex
          , D.field "result" decresult
          , D.field "compiled" deccompiled
          , decmode
          , decCError
          ]

decCError =
  P.decode SCError
    |> P.required "compileError" D.string
decSAlert =
  P.decode SAlert
    |> P.required "message" D.string
decqok =
  P.decode (\a b -> STrace (SQueryOk a b))
    |> P.required "inputs" (D.list D.string)
    |> P.required "outputs" (D.list D.string)
decqno =
  P.decode (STrace << SQueryNo)
    |> P.required "inputs" (D.list D.string)
deccheck =
  P.decode (STrace << SCheck)
    |> P.required "uri" D.string
decresult =
  P.decode (STrace << SResult)
    |> P.required "uri" D.string
deccex =
  P.decode (STrace << SCex)
    |> P.required "inputs" (D.list D.string)
deccompiled =
  P.decode (\_ -> SCompiled)
    |> P.hardcoded "asdf"
decmode = 
  let toDecoder : String -> D.Decoder ServerMsg
      toDecoder s = case s of
        "static" -> D.succeed (SHello Static)
        "custom" -> D.succeed (SHello Custom)
        _ -> D.fail "Not a valid server mode."
  in P.decode toDecoder
    |> P.required "hello" D.string
    |> P.resolve

wsUrl : String -> String
wsUrl hostname = "ws://" ++ hostname ++ ":" ++ toString serverPort

tryDecServerMsg s = case D.decodeString decodeSMsg s of
  Ok m -> ServerMsg m
  Err _ -> BadServerMsg s

subscriptions : Model -> Sub Msg
subscriptions model = WebSocket.listen (wsUrl model.netConf.loc) tryDecServerMsg

serverReq : NetConf -> LP -> Cmd Msg
serverReq nc lp =
  WebSocket.send
    (wsUrl nc.loc)
    (encode 2 (Json.Encode.object
                 [ ("name", Json.Encode.string lp.name)
                 , ("lp", string lp.lpText)
                 ]))
