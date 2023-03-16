open Core
open SessionPi
open Async
(* Wrap them up in printing functions *)
(* Iterate over the tests and print *)

let main () : unit Deferred.t =
  let chanMap = Map.empty (module String) in
  let (c, m) = Chan.newPiChan chanMap "test" Pi.LitString in
  Chan.sendPi m c.port1 "Hello" >>= fun () ->
  Chan.recvPi m c.port2 >>= fun x -> printf "%s" x; return () 

let () =
  Command.async ~summary:"" (Command.Param.return main)
  |> Command_unix.run
