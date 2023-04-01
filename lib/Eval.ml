open Core
open Async
open Pi
open GV

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
       >>> (function
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
  (* NOTE: cannot close channels here as the lifetime of
		 	 a channel might be extended via sends through
			 pipes; TODO: think about some reference counting
			 mechanism to free unnecessary pipes *)
  (*>>| (fun v -> Chan.closePi !globalMap mangled_chan; v)*)
  | Send (chanVar, e) ->
    (* get the mangled name corresponding to this polarity *)
    let mangled_chan = mangledChanName varMap chanVar in
    (* send data through channel *)
    (match e with
     | Str s -> Chan.sendPi !globalMap mangled_chan (Strg s)
     | ChanVar chanVar ->
       (* get deferred value of the channel to be sent *)
       let mangled_chan_send = mangledChanName varMap chanVar in
       let deferred_chan =
         match Map.find !globalMap mangled_chan_send with
         | None -> raise (Failure "bad name")
         | Some v -> v
       in
       (* once determined, write *)
       deferred_chan >>= fun v -> Chan.sendPi !globalMap mangled_chan v
     | _ -> (* TODO *) raise (Failure "Send not implemented for this type"))
    (* once write has completed, return *)
    >>| fun _ -> varMap
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
    >>= (function
    | PiChan (x, y, z, t) ->
      (globalMap
         := match
              Map.add
                !globalMap
                ~key:(-mangled_var)
                ~data:(Deferred.return (PiChan (z, t, x, y)))
            with
            | `Ok new_globalMap -> new_globalMap
            | `Duplicate -> raise (Failure "shadowing variable"));
      (* force the return into the monadic computation to avoid race conditions *)
      Deferred.return new_varMap
    | _ -> Deferred.return new_varMap)
  | _ -> (* TODO *) raise (Failure "language construct not implemented")

and igneval x = ignore (eval x)

let beta_reduce (e1 : expr) (v : string) (e2 : expr) =
  ignore (e1, v, e2);
  raise (Failure "TODO")
;;

let rec eval_expression (localMap, globalMap, ast) : expr Deferred.t =
  match ast with
  | EValue value -> Deferred.return (EValue value)
  | ETup (e1, e2) ->
    let def_e1 = eval_expression (localMap, globalMap, e1) in
    let def_e2 = eval_expression (localMap, globalMap, e2) in
    def_e1
    >>= fun def_e1 -> def_e2 >>= fun def_e2 -> ETup (def_e1, def_e2) |> Deferred.return
  | EApp (f, arg) ->
    eval_expression (localMap, globalMap, f)
    >>= (function
    | EValue ev ->
      (function
       | Constant c ->
         ignore c;
         raise (Failure "TODO")
       | Lambda (Variable v, e) ->
         eval_expression (localMap, globalMap, arg)
         >>= fun whnf_arg ->
         eval_expression (localMap, globalMap, beta_reduce whnf_arg v e)
       | _ -> raise (Failure "this should have failed the type checking"))
        ev
    | _ -> raise (Failure "this should have failed the type checking"))
  | _ -> raise (Failure "TODO")
;;

let rec eval_configuration (localMap, globalMap, ast) =
  match ast with
  | CExpr e -> ignore (eval_expression (localMap, globalMap, e))
  | CPar (c1, c2) ->
    let p = spawn_thread eval_configuration (localMap, globalMap, c1) in
    eval_configuration (localMap, globalMap, c2);
    Core_thread.join p
  | _ -> raise (Failure "TODO")
;;
