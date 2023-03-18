open Core
open SessionPi
open Async
open AstPrint

let main () =
  (* TODO: build parser that makes ASTs *)
  let pi1 =
    "(ν z) ((ν x) (x-!<z+> | x+?(y) . y-!<x+> . x+?(t) . print t) | z+?(v) . v-!</hello>)"
  in
  let lexbuf = Lexing.from_string pi1 in
  let ast = Parser.pi Lexer.tokenize lexbuf in
  print_endline "AST: ";
  print_endline (printPi ast);
  print_endline "\n ========= \n";
  print_endline "Execution:";
  ignore
    (Eval.eval
       ( (* empty local varMap *)
         Map.empty (module String)
       , (* ref to empty globalMap *)
         ref (Map.empty (module Int))
       , (* ref to last used mangled name *)
         ref 0
       , (* ast *)
         ast ))
;;

let () =
  main ();
  never_returns (Scheduler.go ())
;;
