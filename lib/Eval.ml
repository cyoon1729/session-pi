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
  | New (namex, NChan ts, p) ->
    (* TC-NEW *)
    let gamma' = gamma |> Map.Poly.add_exn ~key:(Name namex) ~data:(Pi.NChan ts) in
    eval gamma' x p
  | New (namex, SType s, p) ->
    (* TC-NEWS *)
    let gamma' =
      gamma
      |> Map.Poly.add_exn ~key:(Plus namex) ~data:(Pi.SType s)
      |> Map.Poly.add_exn ~key:(Mins namex) ~data:(Pi.SType (Stype.dual s))
    in
    let x' = Set.Poly.add (Set.Poly.add x (Plus namex)) (Mins namex) in
    let y = eval gamma' x' p in
    let xplus = Set.Poly.mem y (Plus namex) in
    let xmins = Set.Poly.mem y (Mins namex) in
    let ended = Stype.sTypeSubC Set.Poly.empty Set.Poly.empty SEnd s in
    (match xplus, xmins, ended with
     | true, true, true ->
       let y' = Set.Poly.remove y (Plus namex) in
       let y'' = Set.Poly.remove y' (Mins namex) in
       y''
     | false, _, _ -> raise (Failure "channel not used completely")
     | _, false, _ -> raise (Failure "channel not used completely")
     | _, _, false -> raise (Failure "channol cannot have end type"))
  | PInput (namex, ys, p) ->
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
    let subtype_vars ys ts : bool =
      ys
      |> List.map ~f:snd
      |> List.for_all2_exn ~f:(Stype.tTypeSubC Set.Poly.empty Set.Poly.empty) ts
    in
    let y =
      match
        ( Map.Poly.find gamma (Name namex)
        , Map.Poly.find gamma (Plus namex)
        , Map.Poly.find gamma (Mins namex) )
      with
      | Some (NChan ts), _, _ when subtype_vars ys ts ->
        (* TC-IN *)
        eval gamma' x' p
      | Some (SType (SInput (ts, s))), _, _ when subtype_vars ys ts ->
        (* TC-INS1 *)
        let gamma'' = Map.Poly.set gamma' ~key:(Name namex) ~data:(SType s) in
        let y = eval gamma'' x' p in
        if Set.Poly.mem x (Name namex) && Set.Poly.mem y (Name namex)
        then y
        else raise (Failure "channel name not available or not used")
      | _, Some (SType (SInput (ts, s))), _ when subtype_vars ys ts ->
        (* TC-INS2 *)
        let gamma'' = Map.Poly.set gamma' ~key:(Plus namex) ~data:(SType s) in
        let x'' = Set.Poly.remove x (Mins namex) in
        let y = eval gamma'' x'' p in
        if Set.Poly.mem x (Plus namex) && Set.Poly.mem y (Plus namex)
        then y
        else raise (Failure "channel name not available or not used")
      | _, _, Some (SType (SInput (ts, s))) when subtype_vars ys ts ->
        (* TC-INS3 *)
        let gamma'' = Map.Poly.set gamma' ~key:(Mins namex) ~data:(SType s) in
        let x'' = Set.Poly.remove x (Plus namex) in
        let y = eval gamma'' x'' p in
        if Set.Poly.mem x (Mins namex) && Set.Poly.mem y (Mins namex)
        then y
        else raise (Failure "channel name not available or not used")
      | _ -> raise (Failure "TODO")
    in
    if Set.Poly.is_subset ySess ~of_:y
    then Set.Poly.diff y ySess
    else raise (Failure "in-names not used completely")
  | POutput (namex, ys, p) -> 
    ignore (namex, ys, p);
    raise (Failure "TODO") (* TODO *)
;;
