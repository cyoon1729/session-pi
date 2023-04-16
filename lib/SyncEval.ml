open Base
open Core

type PeerMap = (Pi.label, Pi.process list, String.comparator_witness) Map.t

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


(* From current evaluation context, separate processes that communicate
 * and those that don't
 * e.g. [PInput, POutput, PBranch, PChoice], [Par, Rep, New, PEnd]
 *)
let splitContext (procs: Pi.process list) : (Pi.process list) * (Pi.process list) =
  let isCommunicating p =
    match p with
    | PEnd | Par (_, _) | New (_, _, _) | Rep _ -> false 
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
  let peerMap = List.fold_left procs (Map.empty (module String)) buildPeerMap
  in List.filter (Map.to_alist PeerMap peerMap) (fun (_, v) -> v)

(* Reduces `isCommunicating` processes, returns a list of processes
 * after running one step of reduction (or just the process if it can't
 * be evaluated yet
 *)
let reducePeers (peerList: (Pi.process list) list) : (Pi.process list) =
  let addTwo (p, q) b = p :: q :: b in
  let rec reducePeers' ps builder = 
    match ps with 
    | [p] :: tl -> reducePeers' tl (p :: builder)
    | [p; q] :: tl -> reducePeers' tl (addTwo (reduceComm p q) builder) 
    | [] -> builder
    | _ -> raise Failure ("can't happen")
  in reducePeers' peerList []

(* Reduces !`isCommunicating` processes. The name is a misnomer, though, since
 * we don't actually fully reduce via the operational semantics.
 * e.g. for Par(p, q), we just unwrap the two parallel processes for them to
 *      be evaluated in the next round.
 *)
let reduceNotCommunicating (procs: Pi.process list) : (Pi.process list) =
  let rec reduceNotComm ps builder =
    match ps with
    | Par (p, q) :: tl -> reduceNotComm tl (p :: q :: builder) 
    | Rep p :: tl -> reduceNotComm tl (p :: Rep p :: builder) 
    | (New (c, ty, p) as newChan) :: tl -> reduceNotComm tl (reduceNew newChan :: builder) 
    | PEnd :: tl -> reduceNotComm tl builder 
    | [] -> builder
    | _ -> raise Failure ("can't happen")
  in reduceNotComm procs []

(*
 * `tail` function for sesstion types, Fig 7 of paper
 *)
let styTail (sty: Pi.sType) (label: Pi.label) : Pi.sType =
  match sty with
  | SInput (_, sty') -> sty'
  | SOutput (_, sty') -> sty'
  | SBranch (branches) -> styChooseBranch branches choice
  | SChoice (branches) -> styChooseBranch branches choice
  | SMu (tv, sty') -> styTail (stySubst sty' tv sty) label
  | SEnd -> raise Failure("Cannot take tail of a terminated session")
  | STypeVar (tv) -> raise Failure("Can't reduce a typeVar")
