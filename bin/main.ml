open SessionPi.Pi
open SessionPi.SyncEval
open SessionPi.Print

let () =
  let p = Par(PInput("c", [("x", Int)], PEnd), POutput("c", [DataInt 1],PEnd)) in
  let onestep = reduceOneStep [p] in 
  let twostep = reduceOneStep onestep in
  print_endline "result:";
  print_endline (printContext onestep);
  print_endline (printContext twostep)
  
