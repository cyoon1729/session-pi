open Core
open Stdio
open SessionPi
open Check

let dir_path_pass = "test-checker/pass/"
let dir_path_fail = "test-checker/fail/"

let check_test test =
  printf "%-30s: " test;
  let contents = In_channel.read_all test in
  let lexbuf = Lexing.from_string contents in
  let ast = Parser.pi Lexer.tokenize lexbuf in
  Check.check ast
;;

let check_test_pass test =
  try
    check_test test;
    printf "Ok\n"
  with
  | Failure m -> printf "\027[31mFail (%s)\n\027[0m" m
;;

let check_test_fail test =
  try
    check_test test;
    printf "\027[31mFail\n\027[0m"
  with
  | Failure m -> printf "Ok (%s)\n" m
;;

let () =
  printf "\n";
  Sys_unix.ls_dir dir_path_pass
  |> List.filter ~f:(fun x -> not @@ String.is_prefix ~prefix:"." x)
  |> List.sort ~compare:String.compare
  |> List.iter ~f:(fun test -> check_test_pass (dir_path_pass ^ test));
  Sys_unix.ls_dir dir_path_fail
  |> List.filter ~f:(fun x -> not @@ String.is_prefix ~prefix:"." x)
  |> List.sort ~compare:String.compare
  |> List.iter ~f:(fun test -> check_test_fail (dir_path_fail ^ test))
;;
