(*
 * TODO:
 *  - Implement each reduction rule
 *  - Define termination conditions
 *  - Implement substitutions and the congruence rules: when to use/apply congruence rules?
 *  - Evalution one step of the reduction of the whole context
 *  - Evaluate whole program until termination
 *)

open Base
open Core

type peerMap = (Pi.label, Pi.process list, String.comparator_witness) Map.t

(* One step of reduction. Works roughly as followed
 * - Split the list of "concurrent" processes (i.e. the context) into 
 *   processes that are communicating with another process, and those that aren't
 * - Reduce the two lists separately and concatenate them.
 *)
let reduceOneStep (procs: Pi.process list) : Pi.process list =
  let (comm, notComm) = splitContext procs in
  let comm' = reduceCommunicatingPeers comm in
  let notComm' = reduceNotCommunicating notComm in
  List.append comm' notComm'

(* Determines if the context can no longer be reduced
 * If there are no processes, or all processes in the context is PEnd or a Rep,
 *  then we can terminate
 * Otherwise, there should still remain processes in the context that can be
 *   reduced further
 *)
let canTerminate (procs: Pi.process list) : bool = 
  let rec checkEach ps = 
    match ps with
    | PEnd :: tl | Rep _ :: tl -> checkEach tl 
    | [] -> true
    | _ -> false
  in checkEach procs

(* Reduce until termination (if possible) and return the result *)
let rec reduce (procs: Pi.process list) : Pi.process list =
  match (canTerminate procs) with
  | true -> reduce (reduceOneStep procs)
  | false -> procs

(* From current evaluation context, separate processes that communicate
 * and those that don't
 * e.g. [PInput, POutput, PBranch, PChoice, New(..., communicating process)] (or maybe not new)
 *      [Par, Rep, PEnd, New(.., not communicating process)]
 *)
let splitContext (procs: Pi.process list) : (Pi.process list) * (Pi.process list) =
  let isCommunicating p =
    match p with
    | PEnd | Par (_, _) | Rep _ | New (_, _, _) -> false
    | _ -> true
  in let rec splitContext' ps comm notComm =
    match ps with
    | [] -> (comm, notComm)
    | p :: tl ->
      if isCommunicating p
        then splitContext' tl (p :: comm, notComm)
        else splitContext' tl (comm, p :: notComm)
  in splitContext' procs [] [] 

(* Group processes that communicate each other (by linearity of processes we
 * should only get pairs). Processes who don't yet have a peer in the head
 * of the context yet are left as singletons
 *)
let getPeers (procs: Pi.process) : (Pi.process list) list =
  let extractChanName p =
    match p with
    | PInput (c, _, _) | POutput (c, _, _) | PBranch (c, _) | PChoice (c, _, _) -> c
    | _ -> raise Failure ("can't happen")
  in 
  let buildPeerMap m p =
    let c = extractChanName p in
      match PeerMap.find c with
      | None -> Map.add PeerMap ~key:c ~data:[p] 
      | Some [q] -> Map.add PeerMap ~key:c ~data:[p, q]
      | _ -> raise Failure ("can't happen")
  in
  let peers = List.fold_left procs (Map.empty (module String)) buildPeerMap
  in List.filter (Map.to_alist peerMap peers) (fun (_, v) -> v)

(* Reduces `isCommunicating` processes, returns a list of processes
 * after running one step of reduction (or just the process if it can't
 * be evaluated yet
 *)
let reduceCommunicatingPeers (peerList: (Pi.process list) list) : (Pi.process list) =
  let rec reduceHelper ps builder = 
    match ps with 
    | [p] :: tl -> reduceHelper tl (p :: builder)
    | [p; q] :: tl -> reduceHelper tl (addTwo (reduceComm p q) builder) 
    | [] -> builder
    | _ -> raise Failure ("can't happen")
  in reduceHelper peerList []

(* Reduces `not isCommunicating` processes. The name is a misnomer, though, since
 * we don't actually fully reduce via the reduction rules.
 * e.g. for Par(p, q), we just unwrap the two parallel processes for them to
 *      be evaluated in the next round.
 * NOTE: Maybe we don't need R-New and R-NewS for evaluation? 
 *)
let reduceNotCommunicating (procs: Pi.process list) : (Pi.process list) =
  let rec reduceHelper ps builder =
    match ps with
    | Par (p, q) :: tl -> reduceHelper tl (p :: q :: builder) 
    | Rep p :: tl -> reduceHelper tl (p :: Rep p :: builder) 
    | (New (c, ty, p) as newChan) :: tl -> reduceHelper tl (p :: builder) 
    | PEnd :: tl -> reduceHelper tl builder 
    | [] -> builder
    | _ -> raise Failure ("can't happen")
  in reduceHelper procs []

(* Implements R-Com and R-Select *)
let reduceComm (p: Pi.process) (q: Pi.process) : Pi.process * Pi.process =
  match (p, q) with
  | (PInput(_, ins, p'), POutput(_, outs, q')) -> (subst p' ins outs, q')
  | (POutput(_, _, _), PInput(_, _, _)) -> reduceComm q p
  | (PBranch(_, branches), PChoice(_, lab, p')) -> (chooseBranch branches lab, p')
  | (PChoice(_, _, _), PBranch(_, _)) -> reduceComm q p
  | _ -> raise Failure ("shouldn't happen")

let rec chooseBranch
  (branches: (Pi.label * Pi.process) list)
  (choice: Pi.label)
  : Pi.process = 
  match branches with
  | (lab, branch) :: tl ->
    if lab == choice
      then branch
      else chooseBranch tl choice 
  | _ -> raise Failure ("can't happen")

(* substitution rules pt. 1, fig 6  *)
let substVar (x: Pi.name) (vars: Pi.name) (subs: Pi.data) : Pi.data =
  let rec findSub targ vs ss =
    match (vs, ss) with
    | (v :: vtl, s :: stl) -> if x == v then s else findSub targ vtl stl 
    | ([], []) -> Pi.DataVar targ
    | _ -> raise Failure ("can't happen")
  in findSub x vars subs 

(* substitution rules pt. 2, fig 6  *)
let subst (proc: Pi.process) vars subs : Pi.process = 
  match proc with
  | PEnd -> PEnd
  | Par (p, q) -> Par ((subst p vars subs), (subst p vars subs)) 
  | Rep p -> Rep (subst p vars subs)
  | PInput(c, ins, p) -> PInput (substVar c vars subs, ins, subst p vars subs)
  | POutput(c, outs, p) -> POutput (substVar c vars subs, outs, subst p vars subs)
  | New (c, ty, p) -> New (c, ty, subst p vars subs)
  | PBranch (c, bs) -> PBranch (substVar c vars subs, List.map bs (fun p -> subst p vars subs)) 
  | PSelect (c, sel, p) -> PSelect (substVar c vars subs, sel, subst p vars subs) 

(* `tail` function for sesstion types, Fig 7 of paper *)
let styTail (sty: Pi.sType) (label: Pi.label) : Pi.sType =
  match sty with
  | SInput (_, sty') -> sty'
  | SOutput (_, sty') -> sty'
  | SBranch (branches) -> styChooseBranch branches choice
  | SChoice (branches) -> styChooseBranch branches choice
  | SMu (tv, sty') -> styTail (stySubst sty' tv sty) label
  | SEnd -> raise Failure("Cannot take tail of a terminated session")
  | STypeVar (tv) -> raise Failure("Can't reduce a typeVar")

(* Congruence rules, Fig 4. Might be useless.
 * Given P, return Q that is congruent to P
 *)
let congruence (proc: Pi.process) : Pi.process = 
  match proc with
  | Par (p, Pend) -> p 
  | Par (Pend, p) -> p
  | Par (p, q) -> Par (q, p)
  | Par (p, Par (q, r)) -> Par (Par (p, q), r)
  | Par (Par (p, q), r) -> Par (p, Par (q, r))
  | Rep p -> Par (p, Rep p)
  | Par (New (c, ty, p), q) -> New (c, ty, Par (p, q)) (* need to check c \notin fn(q) and ty != SEnd *)
  | New (c, ty, PEnd) ->
    match ty with
     | SType _ => proc (* no congruence rule defined for this *)
     | _ => PEnd 
  | New (c, SEnd, PEnd) -> PEnd
  | New (c1, ty1, New (c2, ty2, p)) -> New (c2, ty2, New (c1, ty1, p))

(* Silly helper functions *)
let addTwo (p, q) b = p :: q :: b
