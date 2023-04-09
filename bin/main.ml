open Core
open Async
(*open SessionPi*)

let main () = ()

let () =
  main ();
  never_returns (Scheduler.go ())
;;
