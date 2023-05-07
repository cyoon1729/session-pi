open Core

type envName =
  | Name of Pi.name
  | Plus of Pi.name
  | Mins of Pi.name

(* p. 219 *)
let rec check
  (gamma : (envName, Pi.tType) Map.Poly.t)
  (x : envName Set.Poly.t)
  (ast : Pi.process)
  : envName Set.Poly.t
  =
  match ast with
  | PEnd -> tcnil gamma x ast
  | Par _ -> tcpar gamma x ast
  | Rep _ -> tcrep gamma x ast
  | New (_, t, _) when Stype.tTypeLeftRec t ~tvs:Set.Poly.empty ->
    raise (Failure "recursive type")
  | New (_, t, _)
    when not @@ Stype.tTypeClosed t ~tTvs:Set.Poly.empty ~sTvs:Set.Poly.empty ->
    raise (Failure "type not closed")
  | New (c, t, p) ->
    (match Stype.stepLeftRec t with
     | NChan _ as t' -> tcnew gamma x @@ Pi.New (c, t', p)
     | SType _ as t' -> tcnews gamma x @@ Pi.New (c, t', p)
     | _ -> raise (Failure "not a channel type"))
  | PInput _ -> tcin gamma x ast
  | POutput _ -> tcout gamma x ast
  | PBranch _ -> tcoffer gamma x ast
  | PChoice _ -> tcchoose gamma x ast

and tcnil
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
  | _ -> raise (Failure "incorrect function")

and tcpar
  (gamma : (envName, Pi.tType) Map.Poly.t)
  (x : envName Set.Poly.t)
  (ast : Pi.process)
  : envName Set.Poly.t
  =
  match ast with
  | Par (p, q) ->
    (* TC-PAR *)
    let y = check gamma x p in
    let gamma' = Set.Poly.fold y ~init:gamma ~f:Map.Poly.remove in
    let x' = Set.Poly.fold y ~init:x ~f:Set.Poly.remove in
    let z = check gamma' x' q in
    Set.Poly.union y z
  | _ -> raise (Failure "incorrect function")

and tcrep
  (gamma : (envName, Pi.tType) Map.Poly.t)
  (x : envName Set.Poly.t)
  (ast : Pi.process)
  : envName Set.Poly.t
  =
  ignore x;
  match ast with
  | Rep p ->
    (* TC-REP *)
    check gamma Set.Poly.empty p
  | _ -> raise (Failure "incorrect function")

and tcnew
  (gamma : (envName, Pi.tType) Map.Poly.t)
  (x : envName Set.Poly.t)
  (ast : Pi.process)
  : envName Set.Poly.t
  =
  match ast with
  | New (namex, NChan ts, p) ->
    (* TC-NEW *)
    let gamma' = gamma |> Map.Poly.add_exn ~key:(Name namex) ~data:(Pi.NChan ts) in
    check gamma' x p
  | _ -> raise (Failure "incorrect function")

and tcnews
  (gamma : (envName, Pi.tType) Map.Poly.t)
  (x : envName Set.Poly.t)
  (ast : Pi.process)
  : envName Set.Poly.t
  =
  match ast with
  | New (namex, SType s, p) ->
    (* TC-NEWS *)
    let gamma' =
      gamma
      |> Map.Poly.add_exn ~key:(Plus namex) ~data:(Pi.SType s)
      |> Map.Poly.add_exn ~key:(Mins namex) ~data:(Pi.SType (Stype.dual s))
    in
    let x' = Set.Poly.add (Set.Poly.add x (Plus namex)) (Mins namex) in
    let y = check gamma' x' p in
    let xplus = Set.Poly.mem y (Plus namex) in
    let xmins = Set.Poly.mem y (Mins namex) in
    let ended = Stype.sTypeSubC Set.Poly.empty Set.Poly.empty SEnd s in
    (match xplus, xmins, ended with
     | true, true, false ->
       let y' = Set.Poly.remove y (Plus namex) in
       let y'' = Set.Poly.remove y' (Mins namex) in
       y''
     | false, _, _ -> raise (Failure "channel not used completely")
     | _, false, _ -> raise (Failure "channel not used completely")
     | _, _, true ->
       (*print_endline @@ Stype.sTypeToString s;*)
       raise (Failure "channel cannot have end type"))
  | _ -> raise (Failure "incorrect function")

and tcin
  (gamma : (envName, Pi.tType) Map.Poly.t)
  (x : envName Set.Poly.t)
  (ast : Pi.process)
  : envName Set.Poly.t
  =
  match ast with
  | PInput (namex, ys, p) ->
    (* add new names to environment *)
    let gamma' =
      List.fold ys ~init:gamma ~f:(fun gamma (n, t) ->
        Map.Poly.add_exn gamma ~key:(Name n) ~data:t)
    in
    (* get the received session ys *)
    let ySess =
      ys
      |> List.filter_map ~f:(fun (n, t) ->
           match t with
           | Pi.SType _ -> Some (Name n)
           | _ -> None)
      |> Set.Poly.of_list
    in
    (* add session ys to available session names *)
    let x' = Set.Poly.union x ySess in
    let subtype_vars ys ts : bool =
      if List.length ys <> List.length ts
      then raise (Failure "incompatible input list lengths")
      else
        ys
        |> List.map ~f:snd
        |> List.for_all2_exn ~f:(Stype.tTypeSubC Set.Poly.empty Set.Poly.empty) ts
    in
    (* multiplex on input rules *)
    let y =
      match
        ( Map.Poly.find gamma (Name namex)
        , Map.Poly.find gamma (Plus namex)
        , Map.Poly.find gamma (Mins namex) )
      with
      | Some (NChan ts), _, _ when subtype_vars ys ts ->
        (* TC-IN *)
        check gamma' x' p
      | Some (SType (SInput (ts, s))), _, _ when subtype_vars ys ts ->
        (* TC-INS1 *)
        let gamma'' = Map.Poly.set gamma' ~key:(Name namex) ~data:(SType s) in
        let y = check gamma'' x' p in
        if Set.Poly.mem x (Name namex) && Set.Poly.mem y (Name namex)
        then y
        else raise (Failure "channel name not available or not used")
      | _, Some (SType (SInput (ts, s))), _ when subtype_vars ys ts ->
        (* TC-INS2 *)
        let gamma'' = Map.Poly.set gamma' ~key:(Plus namex) ~data:(SType s) in
        let x'' = Set.Poly.remove x (Mins namex) in
        let y = check gamma'' x'' p in
        if Set.Poly.mem x (Plus namex) && Set.Poly.mem y (Plus namex)
        then y
        else raise (Failure "channel name not available or not used")
      | _, _, Some (SType (SInput (ts, s))) when subtype_vars ys ts ->
        (* TC-INS3 *)
        let gamma'' = Map.Poly.set gamma' ~key:(Mins namex) ~data:(SType s) in
        let x'' = Set.Poly.remove x (Plus namex) in
        let y = check gamma'' x'' p in
        if Set.Poly.mem x (Mins namex) && Set.Poly.mem y (Mins namex)
        then y
        else raise (Failure "channel name not available or not used")
      | _ -> raise (Failure "input operation on invalid name")
    in
    if Set.Poly.is_subset ySess ~of_:y
    then Set.Poly.diff y ySess
    else raise (Failure "in-names not used completely")
  | _ -> raise (Failure "incorrect function")

and tcout
  (gamma : (envName, Pi.tType) Map.Poly.t)
  (x : envName Set.Poly.t)
  (ast : Pi.process)
  : envName Set.Poly.t
  =
  match ast with
  | POutput (namex, ys, p) ->
    let find_y_ys gamma x ts =
      (* find the (polarized) names and types of the session-typed ys *)
      let yqs =
        List.zip_exn ys ts
        |> List.filter_map ~f:(fun (y', t) ->
             match y' with
             | Pi.DataVar y ->
               (match
                  ( Map.Poly.find gamma (Name y)
                  , Map.Poly.find gamma (Plus y)
                  , Map.Poly.find gamma (Mins y) )
                with
                | Some (Pi.SType u), _, _
                  when Stype.tTypeSubC Set.Poly.empty Set.Poly.empty (Pi.SType u) t ->
                  Some (Name y, Pi.SType u)
                | _, Some (Pi.SType u), _
                  when Stype.tTypeSubC Set.Poly.empty Set.Poly.empty (Pi.SType u) t ->
                  Some (Plus y, Pi.SType u)
                | _, _, Some (Pi.SType u)
                  when Stype.tTypeSubC Set.Poly.empty Set.Poly.empty (Pi.SType u) t ->
                  Some (Mins y, Pi.SType u)
                | Some u, _, _ ->
                  (* otherwise, not a session type, only check for subtyping *)
                  if Stype.tTypeSubC Set.Poly.empty Set.Poly.empty u t
                  then None
                  else raise (Failure "incompatible output type")
                | None, None, None -> raise (Failure "name not in scope")
                | _ -> None)
             | Pi.DataInt _ ->
               (* otherwise, not a session type, only check for subtyping *)
               if Stype.tTypeSubC Set.Poly.empty Set.Poly.empty Pi.Int t
               then None
               else raise (Failure "incompatible output type"))
      in
      (* remove these session-typed ys from gamma *)
      let gamma' =
        List.fold yqs ~init:gamma ~f:(fun gamma (y, _) -> Map.Poly.remove gamma y)
      in
      (* get the session-typed ys, without associated types *)
      let ySess = yqs |> List.map ~f:fst |> Set.Poly.of_list in
      (* remove session ys from available session names *)
      let x' = Set.Poly.diff x ySess in
      if Set.Poly.is_subset ySess ~of_:x
      then check gamma' x' p, ySess
      else raise (Failure "name not in scope")
    in
    (match
       ( Map.Poly.find gamma (Name namex)
       , Map.Poly.find gamma (Plus namex)
       , Map.Poly.find gamma (Mins namex) )
     with
     | Some (NChan ts), _, _ ->
       (* TC-OUT *)
       let y, ySess = find_y_ys gamma x ts in
       Set.Poly.union y ySess
     | Some (SType (SOutput (ts, s))), _, _ ->
       (* TC-OUTS1 *)
       let gamma' = Map.Poly.set gamma ~key:(Name namex) ~data:(SType s) in
       let y, ySess = find_y_ys gamma' x ts in
       if Set.Poly.mem x (Name namex) && Set.Poly.mem y (Name namex)
       then Set.Poly.union y ySess
       else raise (Failure "channel name not available or not used")
     | _, Some (SType (SOutput (ts, s))), _ ->
       (* TC-OUTS2 *)
       let gamma' = Map.Poly.set gamma ~key:(Plus namex) ~data:(SType s) in
       let x' = Set.Poly.remove x (Mins namex) in
       let y, ySess = find_y_ys gamma' x' ts in
       if Set.Poly.mem x (Plus namex) && Set.Poly.mem y (Plus namex)
       then Set.Poly.union y ySess
       else raise (Failure "channel name not available or not used")
     | _, _, Some (SType (SOutput (ts, s))) ->
       (* TC-OUTS3 *)
       let gamma' = Map.Poly.set gamma ~key:(Mins namex) ~data:(SType s) in
       let x' = Set.Poly.remove x (Plus namex) in
       let y, ySess = find_y_ys gamma' x' ts in
       if Set.Poly.mem x (Mins namex) && Set.Poly.mem y (Mins namex)
       then Set.Poly.union y ySess
       else raise (Failure "channel name not available or not used")
     | _ -> raise (Failure "input operation on invalid name"))
  | _ -> raise (Failure "incorrect function")

and tcoffer
  (gamma : (envName, Pi.tType) Map.Poly.t)
  (x : envName Set.Poly.t)
  (ast : Pi.process)
  : envName Set.Poly.t
  =
  match ast with
  | PBranch (namex, lps) ->
    let envx, lts, x' =
      match
        ( Map.Poly.find gamma (Name namex)
        , Map.Poly.find gamma (Plus namex)
        , Map.Poly.find gamma (Mins namex) )
      with
      | Some (SType (SBranch lts)), _, _ ->
        (* TC-OFFER1 *)
        Name namex, lts, x
      | _, Some (SType (SBranch lts)), _ ->
        (* TC-OFFER2 *)
        Plus namex, lts, Set.Poly.remove x (Mins namex)
      | _, _, Some (SType (SBranch lts)) ->
        (* TC-OFFER2 *)
        Mins namex, lts, Set.Poly.remove x (Plus namex)
      | _ -> raise (Failure "branch operation on invalid name")
    in
    (* get Ys = {Yi | gamma, envx : Ti |-x' pi : Yi} *)
    let lpmap =
      match Map.Poly.of_alist lps with
      | `Ok lpmap -> lpmap
      | `Duplicate_key _ -> raise (Failure "error: duplicate label")
    in
    let ys =
      List.map lts ~f:(fun (l, t) ->
        match Map.Poly.find lpmap l with
        | None -> raise (Failure "missing branch label")
        | Some p ->
          let gamma' = Map.Poly.set gamma ~key:envx ~data:(SType t) in
          check gamma' x' p)
    in
    (* get the unique Y where Ys = {Y} *)
    let y_candidate =
      match List.hd ys with
      | Some y_candidate -> y_candidate
      | None -> raise (Failure "parser should have prevented this")
    in
    let y =
      List.fold ys ~init:y_candidate ~f:(fun y_candidate y ->
        if Set.Poly.equal y y_candidate
        then y_candidate
        else raise (Failure "inconsistent branches"))
    in
    (* check that channel used completely *)
    if Set.Poly.mem x envx && Set.Poly.mem y envx
    then y
    else raise (Failure "channel name not available or not used")
  | _ -> raise (Failure "incorrect function")

and tcchoose
  (gamma : (envName, Pi.tType) Map.Poly.t)
  (x : envName Set.Poly.t)
  (ast : Pi.process)
  : envName Set.Poly.t
  =
  match ast with
  | PChoice (namex, l, p) ->
    let envx, lts, x' =
      match
        ( Map.Poly.find gamma (Name namex)
        , Map.Poly.find gamma (Plus namex)
        , Map.Poly.find gamma (Mins namex) )
      with
      | Some (SType (SChoice lts)), _, _ ->
        (* TC-CHOOSE1 *)
        Name namex, lts, x
      | _, Some (SType (SChoice lts)), _ ->
        (* TC-CHOOSE2 *)
        Plus namex, lts, Set.Poly.remove x (Mins namex)
      | _, _, Some (SType (SChoice lts)) ->
        (* TC-CHOOSE2 *)
        Mins namex, lts, Set.Poly.remove x (Plus namex)
      | _ -> raise (Failure "choice operation on invalid name")
    in
    (* get type of label *)
    let ltmap =
      match Map.Poly.of_alist lts with
      | `Ok ltmap -> ltmap
      | `Duplicate_key _ -> raise (Failure "error: duplicate label")
    in
    let t =
      match Map.Poly.find ltmap l with
      | None -> raise (Failure "missing branch label")
      | Some t -> t
    in
    (* update env and get y *)
    let gamma' = Map.Poly.set gamma ~key:envx ~data:(SType t) in
    let y = check gamma' x' p in
    (* check that channel used completely *)
    if Set.Poly.mem x envx && Set.Poly.mem y envx
    then y
    else raise (Failure "channel name not available or not used")
  | _ -> raise (Failure "incorrect function")
;;
