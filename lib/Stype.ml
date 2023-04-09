open Core

(* p. 198 *)
let rec dual (s : Pi.sType) : Pi.sType =
  match s with
  | STypeVar x -> STypeVar x
  | SEnd -> SEnd
  | SInput (ins, s') -> SOutput (ins, dual s')
  | SOutput (outs, s') -> SInput (outs, dual s')
  | SBranch labs -> SChoice (List.map ~f:(fun (l, s') -> (l, dual s')) labs)
  | SChoice labs -> SBranch (List.map ~f:(fun (l, s') -> (l, dual s')) labs)
  | SMu (tv, s') -> SMu (tv, dual s')
