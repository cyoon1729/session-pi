open Stdio
open SessionPi
open Check

let () =
  let filename = "a.txt" in
  let contents = In_channel.read_all filename in
  let lexbuf = Lexing.from_string contents in
  let ast = Parser.pi Lexer.tokenize lexbuf in
  Check.check ast
;;
