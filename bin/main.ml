open Core
open SessionPi
open Async


let writer chanMap port =
  Chan.sendPi chanMap port "Hello"

let reader chanMap port = 
  Chan.recvPi chanMap port >>=
  fun x -> printf "%s\n" x;
  return ()

let main () =
  let chanMap = Map.empty (module String) in
  let (c, m) = Chan.newPiChan chanMap "test" Pi.LitString in
  ignore (Core_thread.create (fun _ -> writer m c.port1) ());
  return ()

(* This works, but we want to call sendPi and recvPi in separate threads
let main () : unit Deferred.t =
  let chanMap = Map.empty (module String) in
  let (c, m) = Chan.newPiChan chanMap "test" Pi.LitString in
  Chan.sendPi m c.port1 "Hello" >>= fun () ->
  Chan.recvPi m c.port2 >>= fun x -> printf "%s" x; return () 
*)

let () =
  Command.async ~summary:"" (Command.Param.return main)
  |> Command_unix.run
