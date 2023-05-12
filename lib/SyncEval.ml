(*
 * TODO:
 *  - Implement each reduction rule
 *  - Define termination conditions
 *  - Implement substitutions and the congruence rules: when to use/apply congruence rules?
 *  - Evalution one step of the reduction of the whole context
 *  - Evaluate whole program until termination
 *)
open Pi
open Print
open Base
open Core

type peerMap = (Pi.label, Pi.process list, String.comparator_witness) Map.t

type context =
  { active : Pi.process list
  ; reps : Pi.process list
  }

let _ = Random.self_init ()

let makeContext (active: Pi.process list) (reps: Pi.process list) : context =
  { active = active; reps = reps }

(* Silly helper functions *)
let addTwo (p, q) b = p :: q :: b

let randomListElement (procs : Pi.process list) : Pi.process =
  let lstLen = List.length procs in
  let rndIndex = Random.int lstLen in
  let rec getElement lst idx =
    match lst with
    | x :: tl -> if idx = 0 then x else getElement tl (idx - 1)
    | [] -> raise (Failure "can't happen")
  in
  getElement procs rndIndex
;;

let justMapAdd m k v =
  match Map.add m ~key:k ~data:v with
  | `Ok m' -> m'
  | _ -> m
;;

let unzipContext (ctx : context) : Pi.process list * Pi.process list =
  let active, rep = ctx.active, ctx.reps in
  active, rep
;;

(* From current evaluation context, separate processes that communicate
 * and those that don't
 * e.g. [PInput, POutput, PBranch, PChoice, New(..., communicating process)] (or maybe not new)
 *      [Par, Rep, PEnd, New(.., not communicating process)]
 *)
let splitContext (procs : Pi.process list) : Pi.process list * Pi.process list =
  let isCommunicating p =
    match p with
    | PEnd | Par (_, _) | Rep _ | New (_, _, _) -> false
    | _ -> true
  in
  let rec split' ps comm notComm =
    match ps with
    | [] -> comm, notComm
    | p :: tl ->
      if isCommunicating p
      then split' tl (p :: comm) notComm
      else split' tl comm (p :: notComm)
  in
  split' procs [] []
;;

(* Group processes that communicate each other (by linearity of processes we
 * should only get pairs). Processes who don't yet have a peer in the head
 * of the context yet are left as singletons
 *)
let groupPeers (procs : Pi.process list) : Pi.process list list =
  let extractChanName p =
    match p with
    | PInput (c, _, _) | POutput (c, _, _) | PBranch (c, _) | PChoice (c, _, _) -> c
    | _ -> raise (Failure "can't happen")
  in
  let rec findNewTmpName m c =
    match Map.find m c with
    | None -> c
    | Some _ -> findNewTmpName m (c ^ "'")
  in
  let buildPeerMap m p =
    let c = extractChanName p in
    match Map.find m c with
    | None -> Map.add_multi m ~key:c ~data:p
    | Some [ q ] ->
      (match p, q with
       | PInput (_, _, _), POutput (_, _, _)
       | POutput (_, _, _), PInput (_, _, _)
       | PBranch (_, _), PChoice (_, _, _)
       | PChoice (_, _, _), PBranch (_, _) -> Map.add_multi m ~key:c ~data:p
       | _ -> m)
    | Some [ _; _ ] -> Map.add_multi m ~key:(findNewTmpName m c) ~data:p
    | _ -> raise (Failure "can't happen")
  in
  let peers = List.fold_left procs ~init:(Map.empty (module String)) ~f:buildPeerMap in
  List.map ~f:(fun (_, v) -> v) (Map.to_alist peers)
;;

let rec chooseBranch (branches : (Pi.label * Pi.process) list) (choice : Pi.label)
  : Pi.process
  =
  match branches with
  | (lab, branch) :: tl ->
    if String.equal lab choice then branch else chooseBranch tl choice
  | _ -> raise (Failure "can't happen")
;;

(* substitution rules pt. 1, fig 6  *)
let substVar (x : Pi.name) (vars : Pi.name list) (subs : Pi.data list) : Pi.data =
  let rec findSub targ vs ss =
    match vs, ss with
    | v :: vtl, s :: stl -> if String.equal x v then s else findSub targ vtl stl
    | [], [] -> Pi.DataVar targ
    | _ -> raise (Failure "can't happen")
  in
  findSub x vars subs
;;

(* substitution rules pt. 2, fig 6  *)
let rec subst (proc : Pi.process) vars subs : Pi.process =
  let valDataVar (d : Pi.data) =
    match d with
    | DataVar v -> v
    | _ -> raise (Failure "Trying to unwrap something other than DataVar")
  in
  match proc with
  | PEnd -> PEnd
  | Par (p, q) -> Par (subst p vars subs, subst q vars subs)
  | Rep p -> Rep (subst p vars subs)
  | PInput (c, ins, p) ->
    PInput (valDataVar (substVar c vars subs), ins, subst p vars subs)
  | POutput (c, outs, p) ->
    POutput (valDataVar (substVar c vars subs), outs, subst p vars subs)
  | New (c, ty, p) -> New (c, ty, subst p vars subs)
  | PBranch (c, bs) ->
    PBranch
      ( valDataVar (substVar c vars subs)
      , List.map bs ~f:(fun (l, b) -> l, subst b vars subs) )
  | PChoice (c, sel, p) ->
    PChoice (valDataVar (substVar c vars subs), sel, subst p vars subs)
;;

(* Determines if the context can no longer be reduced
 * If there are no processes, or all processes in the context is PEnd or a Rep,
 *  then we can terminate
 * Otherwise, there should still remain processes in the context that can be
 *   reduced further
 *)
let canTerminate (ctx : context) : bool =
  let active, _ = unzipContext ctx in
  let rec checkEach ps =
    match ps with
    | PEnd :: tl | Rep _ :: tl -> checkEach tl
    | [] -> true
    | _ -> false
  in
  checkEach active
;;

(* Assuming all current active processes are blocked, 
 * nondeterministically choose a Rep process that will unblock
 * an active process
 *)
let sampleRep (ctx : context) : context * bool =
  let communicating procs =
    let communicating' p =
      match p with
      | PInput (_, _, _) | PBranch (_, _) | POutput (_, _, _) | PChoice (_, _, _) -> true
      | _ -> false
    in
    List.filter ~f:(fun p -> communicating' p) procs
  in
  let getCandidates ps1 ps2 =
    (* is there anything in ps2 that can unblock anything in ps1 *)
    let chansWithPolarity p =
      match p with
      | PInput (c, _, _) -> c ^ "-"
      | PBranch (c, _) -> c ^ "&-"
      | POutput (c, _, _) -> c ^ "+"
      | PChoice (c, _, _) -> c ^ "&+"
      | _ -> raise (Failure "probably shouldn't happen")
    in
    let getChanOfPeer p =
      match p with
      | PInput (c, _, _) -> c ^ "+"
      | PBranch (c, _) -> c ^ "&+"
      | POutput (c, _, _) -> c ^ "-"
      | PChoice (c, _, _) -> c ^ "&-"
      | _ -> raise (Failure "probably shouldn't happen")
    in
    let canUnblock cset c = Set.mem cset c in
    let ps1Chans = List.map ~f:(fun p -> chansWithPolarity p) ps1 in
    let ps1cset = Set.of_list (module String) ps1Chans in
    List.filter ~f:(fun p -> canUnblock ps1cset (getChanOfPeer p)) ps2
  in
  let active, reps = unzipContext ctx in
  let candidates = getCandidates (communicating active) (communicating reps) in
  match candidates with
  | [] -> (ctx, false)
  | [x] -> ({ active = List.append active [x]; reps = reps }, true)
  | _ -> ({ active = List.append active [ randomListElement candidates ]; reps = reps }, true) 
;;

(* Implements R-Com and R-Select *)
let rec reduceComm (p : Pi.process) (q : Pi.process) : Pi.process * Pi.process =
  match p, q with
  | PInput (_, ins, p'), POutput (_, outs, q') ->
    let vars = List.map ~f:(fun (v, _) -> v) ins in
    subst p' vars outs, q'
  | POutput (_, _, _), PInput (_, _, _) -> reduceComm q p
  | PBranch (_, branches), PChoice (_, lab, p') -> chooseBranch branches lab, p'
  | PChoice (_, _, _), PBranch (_, _) -> reduceComm q p
  | _ -> raise (Failure "shouldn't happen")
;;

(* Reduces `isCommunicating` processes, returns a list of processes
 * after running one step of reduction (or just the process if it can't
 * be evaluated yet
 *)
let reduceCommunicatingPeers (peerList : Pi.process list list) : Pi.process list * bool =
  let rec reduceHelper ps builder reduced =
    match ps with
    | [ p ] :: tl -> reduceHelper tl (p :: builder) false
    | [ p; q ] :: tl -> reduceHelper tl (addTwo (reduceComm p q) builder) true
    | [] -> builder, reduced
    | _ -> raise (Failure "can't happen")
  in
  reduceHelper peerList [] false
;;

(* Reduces `not isCommunicating` processes. The name is a misnomer, though, since
 * we don't actually fully reduce via the reduction rules.
 * e.g. for Par(p, q), we just unwrap the two parallel processes for them to
 *      be evaluated in the next round.
 * NOTE: Maybe we don't need R-New and R-NewS for evaluation? 
 *)
let reduceNotCommunicating (procs : Pi.process list)
  : Pi.process list * Pi.process list * bool
  =
  let rec reduceHelper ps builder reps reduced =
    match ps with
    | Par (p, q) :: tl -> reduceHelper tl (p :: q :: builder) reps true
    | Rep p :: tl -> reduceHelper tl builder (p :: reps) true
    | New (_, _, p) :: tl -> reduceHelper tl (p :: builder) reps true
    | PEnd :: tl -> reduceHelper tl builder reps reduced
    | [] -> builder, reps, reduced
    | _ -> raise (Failure "can't happen")
  in
  reduceHelper procs [] [] false
;;

(* One step of reduction. Works roughly as followed
 * - Split the list of "concurrent" processes (i.e. the context) into 
 *   processes that are communicating with another process, and those that aren't
 * - Reduce the two lists separately and concatenate them.
 *)
let reduceOneStep (ctx : context) : context * bool =
  let active, reps = unzipContext ctx in
  let comm, notComm = splitContext active in
  let comm', commReduced = reduceCommunicatingPeers (groupPeers comm) in
  let notComm', reducedReps, notCommReduced = reduceNotCommunicating notComm in
  let active' = List.append comm' notComm' in
  let reps' = List.append reps reducedReps in
  let reduced = commReduced || notCommReduced in
  { active = active'; reps = reps' }, reduced
;;

(* Reduce until termination (if possible) and return the result *)
let rec reduce (ctx : context) : context =
  match canTerminate ctx with
  | false ->
    let oneStep, reduced = reduceOneStep ctx in
    if reduced
      then reduce oneStep
      else
        (match sampleRep oneStep with
         | (_, false) -> ctx
         | (nxt, _) -> reduce nxt)
  | true -> ctx
;;

(* Reduce until termination (if possible) and return the result *)
let rec debugReduce (ctx : context) : context =
  let active, _ = unzipContext ctx in
  print_endline ("active" ^ printProcessList active);
  match canTerminate ctx with
  | false ->
    let oneStep, reduced = reduceOneStep ctx in
    if reduced
      then debugReduce oneStep
      else
        (match sampleRep oneStep with
         | (_, false) -> ctx
         | (nxt, _) -> debugReduce nxt)
  | true -> ctx
;;
