open Base
open Core

let rec sTypeToString (s : Pi.sType) : string =
  match s with
  | STypeVar x -> x
  | SEnd -> "end"
  | SInput (_, s') -> "?{...}." ^ sTypeToString s'
  | SOutput (_, s') -> "!{...}." ^ sTypeToString s'
  | SBranch labs ->
    labs
    |> List.map ~f:(fun (l, s') -> "(" ^ l ^ ": " ^ sTypeToString s' ^ ")")
    |> List.reduce_exn ~f:( ^ )
  | SChoice labs ->
    labs
    |> List.map ~f:(fun (l, s') -> "(" ^ l ^ ": " ^ sTypeToString s' ^ ")")
    |> List.reduce_exn ~f:( ^ )
  | SMu (tv, s') -> "m " ^ tv ^ "." ^ sTypeToString s'
;;

(* p. 198 *)
let rec dual (s : Pi.sType) : Pi.sType =
  match s with
  | STypeVar x -> STypeVar x
  | SEnd -> SEnd
  | SInput (ins, s') -> SOutput (ins, dual s')
  | SOutput (outs, s') -> SInput (outs, dual s')
  | SBranch labs -> SChoice (List.map ~f:(fun (l, s') -> l, dual s') labs)
  | SChoice labs -> SBranch (List.map ~f:(fun (l, s') -> l, dual s') labs)
  | SMu (tv, s') -> SMu (tv, dual s')
;;

(* substitute a session type into a session type *)
(* s{n/x} *)
let sTypeSub (n : Pi.sType) (x : Pi.typeVar) (s : Pi.sType) : Pi.sType =
  let rec sSub (n : Pi.sType) (x : Pi.typeVar) (s : Pi.sType) : Pi.sType =
    match s with
    | STypeVar y -> if String.equal y x then n else STypeVar y
    | SEnd -> SEnd
    | SInput (ts, s') -> SInput (List.map ~f:(tSub n x) ts, sSub n x s')
    | SOutput (ts, s') -> SOutput (List.map ~f:(tSub n x) ts, sSub n x s')
    | SBranch labs -> SBranch (List.map ~f:(fun (l, s') -> l, sSub n x s') labs)
    | SChoice labs -> SChoice (List.map ~f:(fun (l, s') -> l, sSub n x s') labs)
    | SMu (tv, s') ->
      if String.equal tv x
      then raise (Failure "error: name shadowing")
      else SMu (tv, sSub n x s')
  and tSub (n : Pi.sType) (x : Pi.typeVar) (t : Pi.tType) : Pi.tType =
    match t with
    | Int -> Int
    | TTypeVar y ->
      if String.equal y x then raise (Failure "error: invalid type") else TTypeVar y
    | SType s -> SType (sSub n x s)
    | NChan ts -> NChan (List.map ~f:(tSub n x) ts)
    | TMu (tv, t') ->
      if String.equal tv x
      then raise (Failure "error: name shadowing")
      else TMu (tv, tSub n x t')
  in
  sSub n x s
;;

(* substitute a regular type into a regular type *)
(* t{n/x} *)
let tTypeSub (n : Pi.tType) (x : Pi.typeVar) (t : Pi.tType) : Pi.tType =
  let rec tSub (n : Pi.tType) (x : Pi.typeVar) (t : Pi.tType) : Pi.tType =
    match t with
    | Int -> Int
    | TTypeVar y -> if String.equal y x then n else TTypeVar y
    | SType s -> SType (sSub n x s)
    | NChan ts -> NChan (List.map ~f:(tSub n x) ts)
    | TMu (tv, t') ->
      if String.equal tv x
      then raise (Failure "error: name shadowing")
      else TMu (tv, tSub n x t')
  and sSub (n : Pi.tType) (x : Pi.typeVar) (s : Pi.sType) : Pi.sType =
    match s with
    | STypeVar y ->
      if String.equal y x then raise (Failure "error: invalid type") else STypeVar y
    | SEnd -> SEnd
    | SInput (ts, s') -> SInput (List.map ~f:(tSub n x) ts, sSub n x s')
    | SOutput (ts, s') -> SOutput (List.map ~f:(tSub n x) ts, sSub n x s')
    | SBranch labs -> SBranch (List.map ~f:(fun (l, s') -> l, sSub n x s') labs)
    | SChoice labs -> SChoice (List.map ~f:(fun (l, s') -> l, sSub n x s') labs)
    | SMu (tv, s') ->
      if String.equal tv x
      then raise (Failure "error: name shadowing")
      else SMu (tv, sSub n x s')
  in
  tSub n x t
;;

(* p. 214 *)
let rec sTypeSubC
  (sSigma : (Pi.sType * Pi.sType) Set.Poly.t)
  (tSigma : (Pi.tType * Pi.tType) Set.Poly.t)
  (s1 : Pi.sType)
  (s2 : Pi.sType)
  : bool
  =
  match Set.Poly.mem sSigma (s1, s2) with
  | true -> true (* AS-ASSUMP *)
  | false ->
    (match s1, s2 with
     | SEnd, SEnd -> true (* AS-END *)
     | SMu (x, t), u ->
       (* AS-RECL *)
       sTypeSubC (Set.Poly.add sSigma (s1, s2)) tSigma (sTypeSub s1 x t) u
     | t, SMu (x, u) ->
       (* AS-RECR *)
       sTypeSubC (Set.Poly.add sSigma (s1, s2)) tSigma t (sTypeSub s2 x u)
     | SInput (ts, v), SInput (us, w) ->
       (* AS-INS *)
       if List.length ts <> List.length us
       then raise (Failure "incompatible input list lengths")
       else
         sTypeSubC sSigma tSigma v w
         && List.for_all2_exn ts us ~f:(tTypeSubC sSigma tSigma)
     | SOutput (ts, v), SOutput (us, w) ->
       (* AS-OUTS *)
       if List.length ts <> List.length us
       then raise (Failure "incompatible output list lengths")
       else
         sTypeSubC sSigma tSigma v w
         && List.for_all2_exn us ts ~f:(tTypeSubC sSigma tSigma)
     | SBranch labs1, SBranch labs2 ->
       (* AS-BRANCH *)
       let smap1 =
         match Map.Poly.of_alist labs1 with
         | `Ok smap -> smap
         | `Duplicate_key _ -> raise (Failure "error: duplicate label")
       in
       let smap2 =
         match Map.Poly.of_alist labs2 with
         | `Ok smap -> smap
         | `Duplicate_key _ -> raise (Failure "error: duplicate label")
       in
       Map.Poly.for_alli smap1 ~f:(fun ~key ~data ->
         let s = data in
         match Map.Poly.find smap2 key with
         | None -> false
         | Some t -> sTypeSubC sSigma tSigma s t)
     | SChoice labs1, SChoice labs2 ->
       (* AS-CHOICE *)
       let smap1 =
         match Map.Poly.of_alist labs1 with
         | `Ok smap -> smap
         | `Duplicate_key _ -> raise (Failure "error: duplicate label")
       in
       let smap2 =
         match Map.Poly.of_alist labs2 with
         | `Ok smap -> smap
         | `Duplicate_key _ -> raise (Failure "error: duplicate label")
       in
       Map.Poly.for_alli smap2 ~f:(fun ~key ~data ->
         let t = data in
         match Map.Poly.find smap1 key with
         | None -> false
         | Some s -> sTypeSubC sSigma tSigma s t)
     | _, _ -> false)

(* p. 201 & p. 214 *)
and tTypeSubC
  (sSigma : (Pi.sType * Pi.sType) Set.Poly.t)
  (tSigma : (Pi.tType * Pi.tType) Set.Poly.t)
  (t1 : Pi.tType)
  (t2 : Pi.tType)
  : bool
  =
  match Set.Poly.mem tSigma (t1, t2) with
  | true -> true (* AS-ASSUMP *)
  | false ->
    (match t1, t2 with
     | Int, Int -> true
     | SType s1, SType s2 ->
       (* Here, the paper confuses regular types with session types;
          in order to build the parser, need to add the `SType` wrapper;
          so, we can remove it safely *)
       sTypeSubC sSigma tSigma s1 s2
     | NChan ts, NChan us ->
       (* S-CHAN *)
       if List.length ts <> List.length us
       then raise (Failure "incompatible type list lengths")
       else
         List.for_all2_exn ts us ~f:(fun t u ->
           tTypeSubC sSigma tSigma t u && tTypeSubC sSigma tSigma u t)
     | TMu (x, t), u ->
       (* AS-RECL *)
       tTypeSubC sSigma (Set.Poly.add tSigma (t1, t2)) (tTypeSub t1 x t) u
     | t, TMu (x, u) ->
       (* AS-RECR *)
       tTypeSubC sSigma (Set.Poly.add tSigma (t1, t2)) t (tTypeSub t2 x u)
     | _, _ -> false)
;;

(* Check if session type is closed *)
let rec sTypeClosed
  (s : Pi.sType)
  ~(tTvs : Pi.typeVar Set.Poly.t)
  ~(sTvs : Pi.typeVar Set.Poly.t)
  : bool
  =
  match s with
  | STypeVar tv -> Set.Poly.mem sTvs tv
  | SEnd -> true
  | SInput (ts, s') ->
    sTypeClosed s' ~tTvs ~sTvs
    && ts |> List.map ~f:(tTypeClosed ~tTvs ~sTvs) |> List.reduce_exn ~f:( && )
  | SOutput (ts, s') ->
    sTypeClosed s' ~tTvs ~sTvs
    && ts |> List.map ~f:(tTypeClosed ~tTvs ~sTvs) |> List.reduce_exn ~f:( && )
  | SBranch lts ->
    lts
    |> List.map ~f:snd
    |> List.map ~f:(sTypeClosed ~tTvs ~sTvs)
    |> List.reduce_exn ~f:( && )
  | SChoice lts ->
    lts
    |> List.map ~f:snd
    |> List.map ~f:(sTypeClosed ~tTvs ~sTvs)
    |> List.reduce_exn ~f:( && )
  | SMu (tv, s') -> sTypeClosed s' ~tTvs ~sTvs:(Set.Poly.add sTvs tv)

(* Check if regular type is closed *)
and tTypeClosed
  (t : Pi.tType)
  ~(tTvs : Pi.typeVar Set.Poly.t)
  ~(sTvs : Pi.typeVar Set.Poly.t)
  : bool
  =
  match t with
  | Int -> true
  | TTypeVar tv -> Set.Poly.mem tTvs tv
  | SType s -> sTypeClosed s ~tTvs ~sTvs
  | NChan ts -> ts |> List.map ~f:(tTypeClosed ~tTvs ~sTvs) |> List.reduce_exn ~f:( && )
  | TMu (tv, t') -> tTypeClosed t' ~tTvs:(Set.Poly.add tTvs tv) ~sTvs
;;

(* check if session type is left-recursive e.g. m S. m T. S *)
let rec sTypeLeftRec (s : Pi.sType) ~(tvs : Pi.typeVar Set.Poly.t) : bool =
  match s with
  | STypeVar tv -> Set.Poly.mem tvs tv
  | SEnd -> false
  | SInput (ts, s') ->
    sTypeLeftRec s' ~tvs:Set.Poly.empty
    || ts |> List.map ~f:(tTypeLeftRec ~tvs:Set.Poly.empty) |> List.reduce_exn ~f:( || )
  | SOutput (ts, s') ->
    sTypeLeftRec s' ~tvs:Set.Poly.empty
    || ts |> List.map ~f:(tTypeLeftRec ~tvs:Set.Poly.empty) |> List.reduce_exn ~f:( || )
  | SBranch lts ->
    lts
    |> List.map ~f:snd
    |> List.map ~f:(sTypeLeftRec ~tvs:Set.Poly.empty)
    |> List.reduce_exn ~f:( || )
  | SChoice lts ->
    lts
    |> List.map ~f:snd
    |> List.map ~f:(sTypeLeftRec ~tvs:Set.Poly.empty)
    |> List.reduce_exn ~f:( || )
  | SMu (tv, s') -> sTypeLeftRec s' ~tvs:(Set.Poly.add tvs tv)

(* check if regular type is left-recursive e.g. m S. m T. S *)
and tTypeLeftRec (t : Pi.tType) ~(tvs : Pi.typeVar Set.Poly.t) : bool =
  match t with
  | Int -> false
  | TTypeVar tv -> Set.Poly.mem tvs tv
  | SType s -> sTypeLeftRec s ~tvs:Set.Poly.empty
  | NChan ts ->
    ts |> List.map ~f:(tTypeLeftRec ~tvs:Set.Poly.empty) |> List.reduce_exn ~f:( || )
  | TMu (tv, t') -> tTypeLeftRec t' ~tvs:(Set.Poly.add tvs tv)
;;

(* step type until some stub is reached *)
let rec stepLeftRec (t : Pi.tType) : Pi.tType =
  match t with
  | TMu (tv, t') -> stepLeftRec @@ tTypeSub t tv t'
  | x -> x
;;
