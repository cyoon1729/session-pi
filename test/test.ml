open Eval

let () =
  let tests = [comm1; comm2; branch1; branch2; new1] in
  ignore(List.map debugEval tests)
;;
