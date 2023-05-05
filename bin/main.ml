open Core
open Stdio
open SessionPi

let dir_path_pass = "../test-checker/pass/"
let dir_path_fail = "../test-checker/fail/"

let check_test test = 
  printf "%s: " test;
  let contents = In_channel.read_all test in
  let lexbuf = Lexing.from_string contents in
  let ast = Parser.pi Lexer.tokenize lexbuf in
  ignore(Check.check Map.Poly.empty Set.Poly.empty ast)

let check_test_pass test = 
  try 
    check_test test;
    printf "Ok\n" 
  with
    Failure _ -> printf "Fail\n";;

let check_test_fail test = 
  try 
    check_test test;
    printf "Fail\n" 
  with
    Failure _ -> printf "Ok\n";;

let () =
  printf "\n";
  Sys_unix.ls_dir dir_path_pass
  |> List.iter ~f:(fun test -> check_test_pass (dir_path_pass ^ test));
  Sys_unix.ls_dir dir_path_fail
  |> List.iter ~f:(fun test -> check_test_fail (dir_path_fail ^ test));
