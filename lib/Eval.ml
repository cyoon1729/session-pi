open Core

let eval
  (gamma : (Pi.name, Pi.tType) Map.Poly.t)
  (x : Pi.name Set.Poly.t)
  (ast : Pi.process)
  : Pi.name Set.Poly.t
  =
  ignore (gamma, x, ast);
  Set.Poly.empty
;;
