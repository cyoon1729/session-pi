open Base
open Core

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
(* call as sTypeSubC [] [] t1 t2 *)
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
       sTypeSubC sSigma tSigma v w && List.for_all2_exn ts us ~f:(tTypeSubC sSigma tSigma)
     | SOutput (ts, v), SOutput (us, w) ->
       (* AS-OUTS *)
       sTypeSubC sSigma tSigma v w && List.for_all2_exn us ts ~f:(tTypeSubC sSigma tSigma)
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

and tTypeSubC
  (sSigma : (Pi.sType * Pi.sType) Set.Poly.t)
  (tSigma : (Pi.tType * Pi.tType) Set.Poly.t)
  (t1 : Pi.tType)
  (t2 : Pi.tType)
  : bool
  =
  ignore (sSigma, tSigma, t1, t2);
  true
;;
