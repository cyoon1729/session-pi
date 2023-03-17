open Core
open SessionPi
open Async

let spawn_thread work = 
  Core_thread.create ~on_uncaught_exn:`Print_to_stderr work ()

let writer chanMap port =
  Chan.sendPi chanMap port "Hello"

(* NOTE: so we would think `data` here is type `string`, but its actually `'a Deferred.t`.
 *       how do we get the string so that we can print it?
 * resourcs:
 *  - https://github.com/mwhittaker/distributed-systems-ocaml/blob/master/async/pipes.ml
 *  - https://dev.realworldocaml.org/concurrent-programming.html
 *  - https://ocaml.org/p/async_kernel/v0.10.0/doc/Async_kernel/Deferred/index.html
let reader chanMap port =
  let data = Chan.recvPi chanMap port in
  printf "%s\n" data
*)

let reader chanMap port =
  Chan.recvPi chanMap port >>> fun x -> printf "%s\n" x

let main () =
  let chanMap = Map.empty (module String) in
  let (c, m) = Chan.newPiChan chanMap "test" Pi.LitString in
  let p1 = spawn_thread (fun () -> writer m c.port1) in
  let p2 = spawn_thread (fun () -> reader m c.port2) in
  Core_thread.join p1;
  Core_thread.join p2;
  ()

(* This works, but we want to call sendPi and recvPi in separate threads
let main () : unit Deferred.t =
  let chanMap = Map.empty (module String) in
  let (c, m) = Chan.newPiChan chanMap "test" Pi.LitString in
  Chan.sendPi m c.port1 "Hello" >>= fun () ->
  Chan.recvPi m c.port2 >>= fun x -> printf "%s" x; return () 
*)

let () =
  main ();
  never_returns (Scheduler.go ())

  (*
let () =
  Command.async ~summary:"" (Command.Param.return main)
  |> Command_unix.run
*)
