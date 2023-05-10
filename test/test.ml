open Eval

let () =
  let tests = [comm1; comm2; ] in
  ignore(List.map debugEval tests)
;;
