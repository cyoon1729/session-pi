open Core
open SessionPi
open Async
open Pi


let main () =
  (* TODO: build parser that makes ASTs *)
  let ast =
    New
      ( "c"
      , Compose
          (Send (Plus "c", Str "Hello"), Dot (Recv (Minus "c", "x"), Print (Var "x"))) )
  in
  ignore
    (Eval.eval
       ( Map.empty (module String)
       , (* empty local varMap *)
         ref (Map.empty (module Int))
       , (* ref to empty globalMap *)
         ref 0
       , (* ref to last used mangled name *)
         ast ))
;;

(* ast *)

let () =
  main ();
  never_returns (Scheduler.go ())
;;
