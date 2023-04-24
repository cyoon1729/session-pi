open Core
open SessionPi.Pi
open SessionPi.SyncEval
open SessionPi.Print

let () =
  let p = Par (PInput ("c", [ "x", Int ], PEnd), POutput ("c", [ DataInt 1 ], PEnd)) in
  let red = reduce [ p ] in
  let onestep = reduceOneStep [ p ] in
  let twostep = reduceOneStep onestep in
  print_endline "result:";
  print_endline (printProcessList onestep);
  print_endline (printProcessList twostep);
  print_endline (printProcessList red)
;;
