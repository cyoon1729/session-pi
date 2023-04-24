open Base
open Core
open Pi

let rec strJoin (sep: string) = function
  | [] -> ""
  | [str] -> str
  | ""::strs -> strJoin sep strs
  | str::strs -> str ^ sep ^ " " ^ strJoin sep strs

let rec printProcess (proc: Pi.process) : string =
  let printType _ = "TYPE"
  in
  let printIns ins =
    let vars = List.map ~f:(fun (v, t) -> v ^ ": " ^ (printType t)) ins
    in "[" ^ (strJoin "," vars) ^ "]"
  in
  let printOuts outs =
    let printData d = 
      (match d with
       | DataInt i -> string_of_int i
       | DataVar v -> v
       | DataStr s -> s
       | DataBool b -> string_of_bool b)
    in
    let things = List.map ~f:(fun d -> printData d) outs
    in "[" ^ (strJoin "," things) ^ "]"
  in
  let printBranches branches = 
    strJoin "," (List.map ~f:(fun (l, b) -> l ^ ": " ^ printProcess b) branches)
  in
  match proc with
  | PEnd -> "0"
  | Par (p, q) -> "(" ^ (printProcess p) ^ ") || (" ^ (printProcess q) ^ ")"  
  | Rep p -> "!(" ^ (printProcess p) ^ ")"
  | PInput (c, is, p) -> "?" ^ c ^ (printIns is) ^ "." ^ (printProcess p)
  | POutput (c, os, p) -> "!" ^ c ^ (printOuts os) ^ "." ^ (printProcess p) 
  | New (c, ty, p) -> "(v " ^ c ^ ": " ^ (printType ty) ^ ")" ^ (printProcess p) 
  | PBranch (c, bs) -> c ^ " |> " ^ "{" ^ (printBranches bs) ^ "}"
  | PChoice (c, lab, p) -> c ^ " <| " ^ lab ^ "." ^ (printProcess p)

let printContext (ctx: Pi.process list) : string =
  let procs = List.map ~f:(fun p -> printProcess p) ctx in
  let formatted = strJoin "\n  " procs
  in "[\n  " ^ formatted ^ "\n]"
