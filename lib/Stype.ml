open Base
open Core

module Stype : sig
  val sTypeToString : Pi.sType -> string
  val dual : Pi.sType -> Pi.sType
  val sTypeSubC : Pi.sType -> Pi.sType -> bool
  val tTypeSubC : Pi.tType -> Pi.tType -> bool
  val sTypeClosed : Pi.sType -> bool
  val tTypeClosed : Pi.tType -> bool
  val sTypeLeftRec : Pi.sType -> bool
  val tTypeLeftRec : Pi.tType -> bool
  val stepLeftRec : Pi.tType -> Pi.tType
end = struct
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
  let sTypeSub ~(n : Pi.sType) ~(x : Pi.typeVar) ~(s : Pi.sType) : Pi.sType =
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
      | Bool -> Bool
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
  let tTypeSub ~(n : Pi.tType) ~(x : Pi.typeVar) ~(t : Pi.tType) : Pi.tType =
    let rec tSub (n : Pi.tType) (x : Pi.typeVar) (t : Pi.tType) : Pi.tType =
      match t with
      | Int -> Int
      | Bool -> Bool
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
  let rec _sTypeSubC
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
         _sTypeSubC (Set.Poly.add sSigma (s1, s2)) tSigma (sTypeSub ~n:s1 ~x ~s:t) u
       | t, SMu (x, u) ->
         (* AS-RECR *)
         _sTypeSubC (Set.Poly.add sSigma (s1, s2)) tSigma t (sTypeSub ~n:s2 ~x ~s:u)
       | SInput (ts, v), SInput (us, w) ->
         (* AS-INS *)
         if List.length ts <> List.length us
         then raise (Failure "incompatible input list lengths")
         else
           _sTypeSubC sSigma tSigma v w
           && List.for_all2_exn ts us ~f:(_tTypeSubC sSigma tSigma)
       | SOutput (ts, v), SOutput (us, w) ->
         (* AS-OUTS *)
         if List.length ts <> List.length us
         then raise (Failure "incompatible output list lengths")
         else
           _sTypeSubC sSigma tSigma v w
           && List.for_all2_exn us ts ~f:(_tTypeSubC sSigma tSigma)
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
           | Some t -> _sTypeSubC sSigma tSigma s t)
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
           | Some s -> _sTypeSubC sSigma tSigma s t)
       | _, _ -> false)

  (* p. 201 & p. 214 *)
  and _tTypeSubC
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
       | Bool, Bool -> true
       | SType s1, SType s2 ->
         (* Here, the paper confuses regular types with session types;
          in order to build the parser, need to add the `SType` wrapper;
          so, we can remove it safely *)
         _sTypeSubC sSigma tSigma s1 s2
       | NChan ts, NChan us ->
         (* S-CHAN *)
         if List.length ts <> List.length us
         then raise (Failure "incompatible type list lengths")
         else
           List.for_all2_exn ts us ~f:(fun t u ->
             _tTypeSubC sSigma tSigma t u && _tTypeSubC sSigma tSigma u t)
       | TMu (x, t), u ->
         (* AS-RECL *)
         _tTypeSubC sSigma (Set.Poly.add tSigma (t1, t2)) (tTypeSub ~n:t1 ~x ~t) u
       | t, TMu (x, u) ->
         (* AS-RECR *)
         _tTypeSubC sSigma (Set.Poly.add tSigma (t1, t2)) t (tTypeSub ~n:t2 ~x ~t:u)
       | _, _ -> false)
  ;;

  let sTypeSubC (s1 : Pi.sType) (s2 : Pi.sType) : bool =
    _sTypeSubC Set.Poly.empty Set.Poly.empty s1 s2
  ;;

  let tTypeSubC (t1 : Pi.tType) (t2 : Pi.tType) : bool =
    _tTypeSubC Set.Poly.empty Set.Poly.empty t1 t2
  ;;

  (* Check if session type is closed *)
  let rec _sTypeClosed
    (s : Pi.sType)
    ~(tTvs : Pi.typeVar Set.Poly.t)
    ~(sTvs : Pi.typeVar Set.Poly.t)
    : bool
    =
    match s with
    | STypeVar tv -> Set.Poly.mem sTvs tv
    | SEnd -> true
    | SInput (ts, s') ->
      _sTypeClosed s' ~tTvs ~sTvs
      && ts |> List.map ~f:(_tTypeClosed ~tTvs ~sTvs) |> List.reduce_exn ~f:( && )
    | SOutput (ts, s') ->
      _sTypeClosed s' ~tTvs ~sTvs
      && ts |> List.map ~f:(_tTypeClosed ~tTvs ~sTvs) |> List.reduce_exn ~f:( && )
    | SBranch lts ->
      lts
      |> List.map ~f:snd
      |> List.map ~f:(_sTypeClosed ~tTvs ~sTvs)
      |> List.reduce_exn ~f:( && )
    | SChoice lts ->
      lts
      |> List.map ~f:snd
      |> List.map ~f:(_sTypeClosed ~tTvs ~sTvs)
      |> List.reduce_exn ~f:( && )
    | SMu (tv, s') -> _sTypeClosed s' ~tTvs ~sTvs:(Set.Poly.add sTvs tv)

  (* Check if regular type is closed *)
  and _tTypeClosed
    (t : Pi.tType)
    ~(tTvs : Pi.typeVar Set.Poly.t)
    ~(sTvs : Pi.typeVar Set.Poly.t)
    : bool
    =
    match t with
    | Int -> true
    | Bool -> true
    | TTypeVar tv -> Set.Poly.mem tTvs tv
    | SType s -> _sTypeClosed s ~tTvs ~sTvs
    | NChan ts ->
      ts |> List.map ~f:(_tTypeClosed ~tTvs ~sTvs) |> List.reduce_exn ~f:( && )
    | TMu (tv, t') -> _tTypeClosed t' ~tTvs:(Set.Poly.add tTvs tv) ~sTvs
  ;;

  let sTypeClosed (s : Pi.sType) : bool =
    _sTypeClosed s ~tTvs:Set.Poly.empty ~sTvs:Set.Poly.empty
  ;;

  let tTypeClosed (t : Pi.tType) : bool =
    _tTypeClosed t ~tTvs:Set.Poly.empty ~sTvs:Set.Poly.empty
  ;;

  (* check if session type is left-recursive e.g. m S. m T. S *)
  let rec _sTypeLeftRec (s : Pi.sType) ~(tvs : Pi.typeVar Set.Poly.t) : bool =
    match s with
    | STypeVar tv -> Set.Poly.mem tvs tv
    | SEnd -> false
    | SInput (ts, s') ->
      _sTypeLeftRec s' ~tvs:Set.Poly.empty
      || ts
         |> List.map ~f:(_tTypeLeftRec ~tvs:Set.Poly.empty)
         |> List.reduce_exn ~f:( || )
    | SOutput (ts, s') ->
      _sTypeLeftRec s' ~tvs:Set.Poly.empty
      || ts
         |> List.map ~f:(_tTypeLeftRec ~tvs:Set.Poly.empty)
         |> List.reduce_exn ~f:( || )
    | SBranch lts ->
      lts
      |> List.map ~f:snd
      |> List.map ~f:(_sTypeLeftRec ~tvs:Set.Poly.empty)
      |> List.reduce_exn ~f:( || )
    | SChoice lts ->
      lts
      |> List.map ~f:snd
      |> List.map ~f:(_sTypeLeftRec ~tvs:Set.Poly.empty)
      |> List.reduce_exn ~f:( || )
    | SMu (tv, s') -> _sTypeLeftRec s' ~tvs:(Set.Poly.add tvs tv)

  (* check if regular type is left-recursive e.g. m S. m T. S *)
  and _tTypeLeftRec (t : Pi.tType) ~(tvs : Pi.typeVar Set.Poly.t) : bool =
    match t with
    | Int -> false
    | Bool -> false
    | TTypeVar tv -> Set.Poly.mem tvs tv
    | SType s -> _sTypeLeftRec s ~tvs:Set.Poly.empty
    | NChan ts ->
      ts |> List.map ~f:(_tTypeLeftRec ~tvs:Set.Poly.empty) |> List.reduce_exn ~f:( || )
    | TMu (tv, t') -> _tTypeLeftRec t' ~tvs:(Set.Poly.add tvs tv)
  ;;

  let sTypeLeftRec (s : Pi.sType) : bool = _sTypeLeftRec s ~tvs:Set.Poly.empty
  let tTypeLeftRec (t : Pi.tType) : bool = _tTypeLeftRec t ~tvs:Set.Poly.empty

  (* step type until some stub is reached *)
  let rec stepLeftRec (t : Pi.tType) : Pi.tType =
    match t with
    | TMu (tv, t') -> stepLeftRec @@ tTypeSub ~n:t ~x:tv ~t:t'
    | x -> x
  ;;
end
