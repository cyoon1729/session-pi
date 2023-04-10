open Core

type envName =
  | Name of Pi.name
  | Plus of Pi.name
  | Mins of Pi.name

(* p. 219 *)
let rec eval
  (gamma : (envName, Pi.tType) Map.Poly.t)
  (x : envName Set.Poly.t)
  (ast : Pi.process)
  : envName Set.Poly.t
  =
  match ast with
  | PEnd ->
    (* TC-NIL *)
    Set.Poly.filter x ~f:(fun xp ->
      match Map.Poly.find gamma xp with
      | Some (SType SEnd) -> true
      | _ -> false)
  | Par (p, q) ->
    (* TC-PAR *)
    let y = eval gamma x p in
    let gamma' = Set.Poly.fold y ~init:gamma ~f:Map.Poly.remove in
    let x' = Set.Poly.fold y ~init:x ~f:Set.Poly.remove in
    let z = eval gamma' x' q in
    Set.Poly.union y z
  | Rep p ->
    (* TC-REP *)
    eval gamma Set.Poly.empty p
  | New (tvx, NChan ts, p) ->
    (* TC-NEW *)
    let gamma' = gamma |> Map.Poly.add_exn ~key:(Name tvx) ~data:(Pi.NChan ts) in
    eval gamma' x p
  | New (tvx, SType s, p) ->
    (* TC-NEWS *)
    let gamma' =
      gamma
      |> Map.Poly.add_exn ~key:(Plus tvx) ~data:(Pi.SType s)
      |> Map.Poly.add_exn ~key:(Mins tvx) ~data:(Pi.SType (Stype.dual s))
    in
    let x' = Set.Poly.add (Set.Poly.add x (Plus tvx)) (Mins tvx) in
    let y = eval gamma' x' p in
    let xplus = Set.Poly.mem y (Plus tvx) in
    let xmins = Set.Poly.mem y (Mins tvx) in
    let ended = Stype.sTypeSubC Set.Poly.empty Set.Poly.empty SEnd s in
    (match xplus, xmins, ended with
     | true, true, true ->
       let y' = Set.Poly.remove y (Plus tvx) in
       let y'' = Set.Poly.remove y' (Mins tvx) in
       y''
     | false, _, _ -> raise (Failure "channel not used completely")
     | _, false, _ -> raise (Failure "channel not used completely")
     | _, _, false -> raise (Failure "channol cannot have end type"))
  | PInput (tvx, ys, p) ->
    (* multiplex on input rules *)
    let gamma' =
      List.fold ys ~init:gamma ~f:(fun gamma (n, t) ->
        Map.Poly.add_exn gamma ~key:(Name n) ~data:t)
    in
    let ySess =
      ys
      |> List.filter_map ~f:(fun (n, t) ->
           match t with
           | Pi.SType _ -> Some (Name n)
           | _ -> None)
      |> Set.Poly.of_list
    in
    let x' = Set.Poly.union x ySess in
    let subtype_vars ys ts =
      ys
      |> List.map ~f:snd
      |> List.for_all2_exn ~f:(Stype.tTypeSubC Set.Poly.empty Set.Poly.empty) ts
    in
    let y =
      match
        ( Map.Poly.find gamma (Name tvx)
        , Map.Poly.find gamma (Plus tvx)
        , Map.Poly.find gamma (Mins tvx) )
      with
      | Some (NChan ts), _, _ when subtype_vars ys ts ->
        (* TC-IN *)
        eval gamma' x' p
      | Some (SType (SInput (ts, s))), _, _ when subtype_vars ys ts ->
        (* TC-INS1 *)
        let gamma'' = Map.Poly.set gamma' ~key:(Name tvx) ~data:(SType s) in
        let y = eval gamma'' x' p in
        if Set.Poly.mem x (Name tvx) && Set.Poly.mem y (Name tvx)
        then y
        else raise (Failure "channel name not available or not used")
      | _, Some (SType (SInput (ts, s))), _ when subtype_vars ys ts ->
        (* TC-INS2 *)
        let gamma'' = Map.Poly.set gamma' ~key:(Plus tvx) ~data:(SType s) in
        let x'' = Set.Poly.remove x (Mins tvx) in
        let y = eval gamma'' x'' p in
        if Set.Poly.mem x (Plus tvx) && Set.Poly.mem y (Plus tvx)
        then y
        else raise (Failure "channel name not available or not used")
      | _, _, Some (SType (SInput (ts, s))) when subtype_vars ys ts ->
        (* TC-INS3 *)
        let gamma'' = Map.Poly.set gamma' ~key:(Mins tvx) ~data:(SType s) in
        let x'' = Set.Poly.remove x (Plus tvx) in
        let y = eval gamma'' x'' p in
        if Set.Poly.mem x (Mins tvx) && Set.Poly.mem y (Mins tvx)
        then y
        else raise (Failure "channel name not available or not used")
      | _ -> raise (Failure "TODO")
    in
    if Set.Poly.is_subset ySess ~of_:y
    then Set.Poly.diff y ySess
    else raise (Failure "in-names not used completely")
;;
