open Base
open Core
open Pi

let rec strJoin (sep : string) = function
  | [] -> ""
  | [ str ] -> str
  | "" :: strs -> strJoin sep strs
  | str :: strs -> str ^ sep ^ " " ^ strJoin sep strs
;;

let rec printProcess (proc : Pi.process) : string =
  let rec printType t =
    let rec tylist lst acc =
      match lst with
      | [] -> acc
      | ty :: [] -> (acc ^ (printType ty))
      | ty :: tl -> tylist tl (acc ^ (printType ty) ^ ", ")
    in
    match t with
    | Int -> "Int"
    | Bool -> "Bool"
    | SType st ->
      let rec printSType s =
        let rec stytuplist lst acc =
          match lst with
          | [] -> acc
          | (lb, ty) :: [] -> (acc ^ lb ^ ": " ^ (printSType ty))
          | (lb, ty) :: tl -> stytuplist tl (acc ^ lb ^ ": " ^ (printSType ty) ^ ", ")
        in
        match s with
        | STypeVar stv -> stv
        | SEnd -> "End"
        | SInput (tlst, sty) -> "?[" ^ (tylist tlst "") ^ "]" ^ "." ^ (printSType sty)
        | SOutput (tlst, sty) -> "![" ^ (tylist tlst "") ^ "]" ^ "." ^ (printSType sty)
        | SBranch (sbr) -> "&{" ^ (stytuplist sbr "") ^ "}" 
        | SChoice (sch) -> "+{" ^ (stytuplist sch "") ^ "}"
        | SMu (tv, sty) -> "RecSType: " ^ tv ^ "." ^ (printSType sty)
      in printSType st
    | TTypeVar tv -> tv
    | NChan tlst -> "[" ^ (tylist tlst "")  ^ "]"
    | TMu (tv, tty) -> "RecType: " ^ tv ^ "." ^ (printType tty)
  in
  (* let printType _ = "TYPE" in *)
  let printIns ins =
    let vars = List.map ~f:(fun (v, t) -> v ^ ": " ^ printType t) ins in
    "[" ^ strJoin "," vars ^ "]"
  in
  let printOuts outs =
    let printData d =
      match d with
      | DataInt i -> string_of_int i
      | DataVar v -> v
      | DataBool b -> string_of_bool b
    in
    let things = List.map ~f:(fun d -> printData d) outs in
    "[" ^ strJoin "," things ^ "]"
  in
  let printBranches branches =
    strJoin "," (List.map ~f:(fun (l, b) -> l ^ ": " ^ printProcess b) branches)
  in
  match proc with
  | PEnd -> "0"
  | Par (p, q) -> "(" ^ printProcess p ^ ") || (" ^ printProcess q ^ ")"
  | Rep p -> "Rep (" ^ printProcess p ^ ")"
  | PInput (c, is, p) -> "?" ^ c ^ printIns is ^ "." ^ printProcess p
  | POutput (c, os, p) -> "!" ^ c ^ printOuts os ^ "." ^ printProcess p
  | New (c, ty, p) -> "(v " ^ c ^ ": " ^ printType ty ^ ")" ^ printProcess p
  | PBranch (c, bs) -> c ^ " |> " ^ "{" ^ printBranches bs ^ "}"
  | PChoice (c, lab, p) -> c ^ " <| " ^ lab ^ "." ^ printProcess p
;;

let printProcessList (ctx : Pi.process list) : string =
  let procs = List.map ~f:(fun p -> printProcess p) ctx in
  let formatted = strJoin ", " procs in
  "[\n  " ^ formatted ^ "\n]"
;;
