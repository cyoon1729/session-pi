open Pi

let rec printPi pi =
  let printChan c =
    match c with
    | Plus x -> x ^ "+"
    | Minus x -> x ^ "-"
  in
  let printExpr e =
    match e with
    | Num x -> string_of_int x
    | Bool x -> string_of_bool x
    | Str x -> x
    | Var x -> x
    | ChanVar c -> printChan c
  in
  let rec printLabelledPis lps =
    match lps with
    | [] -> ""
    | [ Branch (n, p) ] -> "label: " ^ n ^ " proc: " ^ printPi p
    | Branch (n, p) :: nps ->
      "label: " ^ n ^ " proc: " ^ printPi p ^ ", " ^ printLabelledPis nps
  in
  match pi with
  | Nil -> " Nil "
  | Print e -> " Print " ^ printExpr e
  | Compose (p1, p2) -> "(" ^ printPi p1 ^ ") | (" ^ printPi p2 ^ ")"
  | Seq (p1, p2) -> printPi p1 ^ " . " ^ printPi p2
  | New (c, p) -> "(New " ^ c ^ ") " ^ printPi p
  | Send (c, e) -> " Send (" ^ "chan:" ^ printChan c ^ " data:" ^ printExpr e ^ ")"
  | Recv (c, v) -> " Recv (" ^ "chan:" ^ printChan c ^ " var:" ^ v ^ ")"
  | Select (c, n) -> " Select (" ^ "chan:" ^ c ^ " choice:" ^ n ^ ")"
  | Offer (c, lps) ->
    "Offer (" ^ "chan:" ^ c ^ " branches:[" ^ printLabelledPis lps ^ "])"
;;
