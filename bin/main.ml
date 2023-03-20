open Core
open SessionPi
open Async
open AstPrint

let main () =
  let lexbuf = Lexing.from_channel In_channel.stdin in
  let ast = Parser.pi Lexer.tokenize lexbuf in
  print_endline "AST: ";
  print_endline (printPi ast);
  print_endline "\n ========= \n";
  print_endline "Execution:";
  upon
    (Eval.eval
       ( (* empty local varMap *)
         Map.empty (module String)
       , (* ref to empty globalMap *)
         ref (Map.empty (module Int))
       , (* ref to last used mangled name *)
         ref 0
       , (* ast *)
         ast ))
    (fun _ -> Shutdown.shutdown 0)
;;

let () =
  main ();
  never_returns (Scheduler.go ())
;;
