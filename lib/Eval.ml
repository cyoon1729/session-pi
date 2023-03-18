open Core
open Async
open Pi

(* C-style thread creation; pass function and argument *)
let spawn_thread work arg = Core_thread.create ~on_uncaught_exn:`Print_to_stderr work arg

let mangledChanName varMap chanVar =
  let chan, sign =
    match chanVar with
    | Plus chan -> chan, 1
    | Minus chan -> chan, -1
  in
  let mangled_chan =
    (match Map.find varMap chan with
     | None -> raise (Failure ("not in scope: " ^ chan))
     | Some mangled_chan -> mangled_chan)
    * sign
  in
  mangled_chan
;;

(* evaluate pi caclulus expression *)
let rec eval (varMap, globalMap, last, ast) =
  match ast with
  | Nil -> Deferred.return varMap
  | Print e ->
    (match e with
     | Str s -> printf "%s" s
     | Var var ->
       (* get mangled name of the var *)
       let mangled_var =
         match Map.find varMap var with
         | None -> raise (Failure "bad name")
         | Some v -> v
       in
       (* get deferred value of the var *)
       let v =
         match Map.find !globalMap mangled_var with
         | None -> raise (Failure "bad name")
         | Some v -> v
       in
       (* when decided, print it *)
       v
       >>> fun v ->
       (match v with
        | Strg s -> printf "%s\n" s
        | _ -> (*TODO*) raise (Failure "not implemented"))
     | _ -> (* TODO *) raise (Failure "not implemented"));
    Deferred.return varMap
  | Compose (p, q) ->
    (* execute p and q in parallel; each of them gets its own local 
           varMap, but they share the same reference to the globalMap *)
    let p1 = spawn_thread igneval (varMap, globalMap, last, p) in
    let p2 = spawn_thread igneval (varMap, globalMap, last, q) in
    Core_thread.join p1;
    Core_thread.join p2;
    Deferred.return varMap
  | Seq (p, q) ->
    (* execute p and q sequentially and propagate the varMap
		   through the execution *)
    eval (varMap, globalMap, last, p)
    >>= fun new_varMap -> eval (new_varMap, globalMap, last, q)
  | New (chan, p) ->
    (* add channel as new mangled name *)
    last := !last + 1;
    let mangled_chan = !last in
    let new_varMap =
      match Map.add varMap ~key:chan ~data:mangled_chan with
      | `Ok new_varMap -> new_varMap
      | `Duplicate -> raise (Failure "shadowing name")
    in
    (* create the two polarities of this mangled name,
		   one corresponding to `mangled_chan`, another to
		   `-mangled_chan` *)
    globalMap := Chan.newPiChan !globalMap mangled_chan;
    eval (new_varMap, globalMap, last, p)
  | Send (chanVar, e) ->
    (* get the mangled name corresponding to this polarity *)
    let mangled_chan = mangledChanName varMap chanVar in
    (* send data through channel *)
    (match e with
     | Str s -> Chan.sendPi !globalMap mangled_chan (Strg s)
     | ChanVar chanVar ->
       let mangled_chan_send = mangledChanName varMap chanVar in
       let deferred_chan =
         match Map.find !globalMap mangled_chan_send with
         | None -> raise (Failure "bad name")
         | Some v -> v
       in
       deferred_chan >>> fun v -> Chan.sendPi !globalMap mangled_chan v
     | _ -> (* TODO *) raise (Failure "Send not implemented for this type"));
    Deferred.return varMap
  | Recv (chanVar, var) ->
    (* add variable as new mangled name in local env *)
    last := !last + 1;
    let mangled_var = !last in
    let new_varMap =
      match Map.add varMap ~key:var ~data:mangled_var with
      | `Ok new_varMap -> new_varMap
      | `Duplicate -> raise (Failure "shadowing variable")
    in
    (* get the mangled name corresponding to this polarity *)
    let mangled_chan = mangledChanName varMap chanVar in
    (* receive deferred value and add it in the global map *)
    let var_value = Chan.recvPi !globalMap mangled_chan in
    (globalMap
       := match Map.add !globalMap ~key:mangled_var ~data:var_value with
          | `Ok new_globalMap -> new_globalMap
          | `Duplicate -> raise (Failure "shadowing variable"));
    (* if received a channel, add its other polarity to the map as well *)
    var_value
    >>= fun var_value ->
    (match var_value with
     | PiChan (x, y, z, t) ->
       globalMap
         := (match
               Map.add
                 !globalMap
                 ~key:(-mangled_var)
                 ~data:(Deferred.return (PiChan (z, t, x, y)))
             with
             | `Ok new_globalMap -> new_globalMap
             | `Duplicate -> raise (Failure "shadowing variable"))
     | _ -> ());
    (* force the return into the monadic computation to avoid race conditions *)
    Deferred.return new_varMap
  | _ -> (* TODO *) raise (Failure "language construct not implemented")

and igneval x = ignore (eval x)
