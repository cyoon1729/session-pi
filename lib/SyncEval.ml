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
  { active : Pi.process list;
    reps : Pi.process list;
  }

(* Silly helper functions *)
let addTwo (p, q) b = p :: q :: b

let justMapAdd m k v = 
  match (Map.add m ~key:k ~data:v) with
  | `Ok m' -> m'
  | _ -> m

let unzipContext (ctx : context) : Pi.process list * Pi.process list = 
  let active, rep = ctx.active, ctx.reps
  in (active, rep)

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
  in let rec split' ps comm notComm =
    match ps with
    | [] -> (comm, notComm)
    | p :: tl ->
      if isCommunicating p
        then split' tl (p :: comm) notComm
        else split' tl comm (p :: notComm)
  in split' procs [] [] 
;;

(* Group processes that communicate each other (by linearity of processes we
 * should only get pairs). Processes who don't yet have a peer in the head
 * of the context yet are left as singletons
 *)
let groupPeers (procs : Pi.process list) : (Pi.process list) list =
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
    | None -> justMapAdd m c [p]
    | Some [ q ] ->
      (match p, q with
       | PInput (_, _, _), POutput (_, _, _)
       | POutput (_, _, _), PInput (_, _, _)
       | PBranch (_, _), PChoice (_, _, _)
       | PChoice (_, _, _), PBranch (_, _) -> justMapAdd m c [p; q]
       | _ -> m)
      | Some [ _; _ ] -> justMapAdd m (findNewTmpName m c) [p]
    | _ -> raise (Failure "can't happen")
  in
  let peers = List.fold_left procs ~init:(Map.empty (module String)) ~f:buildPeerMap
  in List.map ~f:(fun (_, v) -> v) (Map.to_alist peers)
;;

let rec chooseBranch
  (branches: (Pi.label * Pi.process) list)
  (choice: Pi.label)
  : Pi.process = 
  match branches with
  | (lab, branch) :: tl ->
    if String.equal lab choice
      then branch
      else chooseBranch tl choice 
  | _ -> raise (Failure "can't happen")

(* substitution rules pt. 1, fig 6  *)
let substVar (x: Pi.name) (vars: Pi.name list) (subs: Pi.data list) : Pi.data =
  let rec findSub targ vs ss =
    match (vs, ss) with
    | (v :: vtl, s :: stl) -> if String.equal x v then s else findSub targ vtl stl 
    | ([], []) -> Pi.DataVar targ
    | _ -> raise (Failure "can't happen")
  in findSub x vars subs 

(* substitution rules pt. 2, fig 6  *)
let rec subst (proc: Pi.process) vars subs : Pi.process =
  let valDataVar (d: Pi.data) = 
    match d with 
    | DataVar (v) -> v
    | _ -> raise (Failure "Trying to unwrap something other than DataVar")
  in
  match proc with
  | PEnd -> PEnd
  | Par (p, q) -> Par ((subst p vars subs), (subst q vars subs)) 
  | Rep p -> Rep (subst p vars subs)
  | PInput(c, ins, p) -> PInput (valDataVar (substVar c vars subs), ins, subst p vars subs)
  | POutput(c, outs, p) -> POutput (valDataVar (substVar c vars subs), outs, subst p vars subs)
  | New (c, ty, p) -> New (c, ty, subst p vars subs)
  | PBranch (c, bs) -> PBranch (valDataVar (substVar c vars subs), List.map bs ~f:(fun (l, b) -> (l, subst b vars subs))) 
  | PChoice (c, sel, p) -> PChoice (valDataVar (substVar c vars subs), sel, subst p vars subs) 

(* Congruence rules, Fig 4. Might be useless.
 * Given P, return Q that is congruent to P
 *)
let congruence (proc: Pi.process) : Pi.process = 
  match proc with
  | Par (p, PEnd) -> p 
  | Par (PEnd, p) -> p
  | Par (p, Par (q, r)) -> Par (Par (p, q), r)
  | Par (Par (p, q), r) -> Par (p, Par (q, r))
  | Par (New (c, ty, p), q) -> New (c, ty, Par (p, q)) (* need to check c \notin fn(q) and ty != SEnd *)
  | Par (p, q) -> Par (q, p)
  | Rep p -> Par (p, Rep p)
  | New (_, SType SEnd, PEnd) -> PEnd
  | New (_, ty, PEnd) ->
    (match ty with
     | SType _ -> proc (* no congruence rule defined for this *)
     | _ -> PEnd) 
  | New (c1, ty1, New (c2, ty2, p)) -> New (c2, ty2, New (c1, ty1, p))
  | _ -> proc (* no congruence rule defined *)

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
let sampleRep (ctx : context) : context = ctx
(*
  let splitProcesses procs = 
    let isInput p = (match p with PInput (_, _, _) -> true | _ -> false) in
    let isBranch p = (match p with PBranch (_, _) -> true | _ -> false) in
    let isOutput p = (match p with POutput (_, _, _) -> true | _ -> false) in
    let isChoice p = (match p with PChoice (_, _, _) -> true | _ -> false) in
    let ins = List.filter procs isInput in
    let branches = List.filter procs isBranch in
    let outs = List.filter procs isOutput in
    let chooses = List.filter procs isChoice in
    (ins, branches, outs, chooses)
  in
  let pickCandidates ps1 ps2 =
    

  let active, reps = ctx in
  let activeIns, activeBranches, activeOuts, activeChooses = splitProcesses active in
  let repIns, repBranches, repOuts, repChooses = splitProcesses reps in
*)

(* Implements R-Com and R-Select *)
let rec reduceComm (p: Pi.process) (q: Pi.process) : Pi.process * Pi.process =
  match (p, q) with
  | (PInput(_, ins, p'), POutput(_, outs, q')) ->
    let vars = List.map ~f:(fun (v, _) -> v) ins
    in (subst p' vars outs, q')
  | (POutput(_, _, _), PInput(_, _, _)) -> reduceComm q p
  | (PBranch(_, branches), PChoice(_, lab, p')) -> (chooseBranch branches lab, p')
  | (PChoice(_, _, _), PBranch(_, _)) -> reduceComm q p
  | _ -> raise (Failure "shouldn't happen")

(* Reduces `isCommunicating` processes, returns a list of processes
 * after running one step of reduction (or just the process if it can't
 * be evaluated yet
 *)
let reduceCommunicatingPeers (peerList : Pi.process list list) : Pi.process list * bool =
  let rec reduceHelper ps builder reduced =
    match ps with
    | [ p ] :: tl -> reduceHelper tl (p :: builder) false
    | [ p; q ] :: tl -> reduceHelper tl (addTwo (reduceComm p q) builder) true
    | [] -> (builder, reduced)
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
let reduceNotCommunicating (procs : Pi.process list) : (Pi.process list) * (Pi.process list) * bool =
  let rec reduceHelper ps builder reps reduced =
    match ps with
    | Par (p, q) :: tl -> reduceHelper tl (p :: q :: builder) reps true
    | Rep p :: tl -> reduceHelper tl builder (p :: reps) true
    | New (_, _, p) :: tl -> reduceHelper tl (p :: builder) reps true
    | PEnd :: tl -> reduceHelper tl builder reps reduced
    | [] -> (builder, reps, reduced)
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
  let (comm', commReduced) = reduceCommunicatingPeers (groupPeers comm) in
  let (notComm', reducedReps, notCommReduced) = reduceNotCommunicating notComm in
  let active' = List.append comm' notComm' in
  let reps' = List.append reps reducedReps in
  let reduced = commReduced || notCommReduced in
  ({ active = active'; reps = reps'; }, reduced)
;;

(* Reduce until termination (if possible) and return the result *)
let rec reduce (ctx : context) : context =
  match canTerminate ctx with
  | false ->
    let (oneStep, reduced) = reduceOneStep ctx in
    if reduced
      then reduce oneStep
      else reduce (sampleRep ctx) 
  | true -> ctx
;;

(* Reduce until termination (if possible) and return the result *)
let rec debugReduce (ctx : context) : context =
  match canTerminate ctx with
  | false ->
    let (oneStep, reduced) = reduceOneStep ctx in
    let active, _ = unzipContext ctx in
    print_endline (printProcessList active);
    if reduced
      then debugReduce oneStep
      else debugReduce (sampleRep ctx) 
  | true -> ctx
;;
