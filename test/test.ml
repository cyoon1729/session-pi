open Core
open SessionPi

(* Wrap them up in printing functions *)
(* Iterate over the tests and print *)

let () =
  let chanMap = Map.empty (module String) in
  let (c, m) = Chan.newPiChan chanMap "test" Pi.LitInt in
  Chan.sendPi m c.port1 234;
