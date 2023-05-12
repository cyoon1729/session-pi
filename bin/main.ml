open Stdio
open SessionPi
open Check

let () =
  print_endline "";
  let filename = Sys.argv.(1) in
  let contents = In_channel.read_all filename in
  let lexbuf = Lexing.from_string contents in
  let ast = Parser.pi Lexer.tokenize lexbuf in
  Check.check ast;
  print_endline "Type check: Ok";
  ignore(SyncEval.debugReduce (SyncEval.makeContext [ast] []));
;;
