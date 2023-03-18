open Core
open SessionPi
open Async
open Pi

let main () =
  (* TODO: build parser that makes ASTs *)

  (*let ast =
    New
      ( "c"
      , Compose
          (Send (Plus "c", Str "Hello"), Dot (Recv (Minus "c", "x"), Print (Var "x"))) )
  in*)
  let ast =
    New
      ( "z"
      , Compose
          ( New
              ( "x"
              , Compose
                  ( Send (Minus "x", ChanVar (Plus "z"))
                  , Dot
                      ( Recv (Plus "x", "y")
                      , Dot
                          ( Send (Minus "y", ChanVar (Plus "x"))
                          , Dot (Recv (Plus "x", "t"), Print (Var "t")) ) ) ) )
          , Dot (Recv (Plus "z", "v"), Send (Minus "v", Str "Hello")) ) )
  in
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
