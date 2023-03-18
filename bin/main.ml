open Core
open SessionPi
open Async
open Pi

(* C-style thread creation; pass function and argument *)
let spawn_thread work arg = 
  Core_thread.create ~on_uncaught_exn:`Print_to_stderr work arg

(* NOTE: so we would think `data` here is type `string`, but its actually `'a Deferred.t`.
 *       how do we get the string so that we can print it?
 * resourcs:
 *  - https://github.com/mwhittaker/distributed-systems-ocaml/blob/master/async/pipes.ml
 *  - https://dev.realworldocaml.org/concurrent-programming.html
 *  - https://ocaml.org/p/async_kernel/v0.10.0/doc/Async_kernel/Deferred/index.html
let reader chanMap port =
  let data = Chan.recvPi chanMap port in
  printf "%s\n" data
*)

let rec eval (varMap, globalMap, last, ast) = 
  match ast with
  | Nil -> varMap
  | Print e ->
      (match e with
      | Str s -> printf "%s" s
	  | Var var -> (* get mangled name of the var *)
				   let mangled_var = (match Map.find varMap var with
             	   					    | None -> raise (Failure("bad name"))
		                		 	    | Some v -> v) in
				   (* get deferred value of the var *)
				   let v = (match Map.find !globalMap mangled_var with
					          | None -> raise (Failure("bad name"))
             				  | Some v -> v) in
				   (* when decided, print it *)
				   v >>> 
				   (fun v -> (match v with
							  	| Strg s -> printf "%s\n" s
							  	| _ -> (*TODO*) raise(Failure("not implemented")))
				   )
	  | _ -> (* TODO *) raise(Failure("not implemented"))
	  );
	  varMap
  | Compose (p, q) ->
		(* execute p and q in parallel; each of them gets its own
		   local varMap, but they share the same reference to the
		   globalMap; to my understanding, race conditions are not
		   possible with these specific threads; TODO: check this *)
		let p1 = spawn_thread igneval (varMap, globalMap, last, p) in
		let p2 = spawn_thread igneval (varMap, globalMap, last, q) in
		Core_thread.join p1;
		Core_thread.join p2;
		varMap
  | Dot (p, q) ->
		(* execute p and q sequentially and propagate the varMap
		   through the execution *)
		let new_varMap = eval (varMap, globalMap, last, p) in
		eval (new_varMap, globalMap, last, q)
  | New (chan, p) ->
		(* add channel as new mangled name *)
		last := !last + 1;
		let mangled_chan = !last in
		let new_varMap = 
  		  (match (Map.add varMap ~key:chan ~data:mangled_chan) with
	         | `Ok new_varMap -> new_varMap
             | `Duplicate -> raise(Failure("shadowing name"))) in

		(* create the two polarities of this mangled name,
		   one corresponding to `mangled_chan`, another to
		   `-mangled_chan` *)
		globalMap := (Chan.newPiChan !globalMap mangled_chan);
		eval (new_varMap, globalMap, last, p);
  | Send (chanVar, e) ->
		(* get the mangled name corresponding to this polarity *)
		let (chan, sign) = (match chanVar with
			      	          | Plus chan -> (chan, 1)
					          | Minus chan -> (chan, -1)) in
		let mangled_chan = 
          (match Map.find varMap chan with
             | None -> raise (Failure("not in scope"))
             | Some mangled_chan -> mangled_chan) * sign in

		(* send data through channel *)
		(match e with
		   | Str s -> (Chan.sendPi !globalMap mangled_chan (Strg s))
		   | _ -> (* TODO *) raise (Failure("Send not implemented for this type")));
		varMap
  | Recv (chanVar, var) ->
		(* TODO: if receiving channel, also add negative to map *)
		(* add variable as new mangled name in local env *)
		last := !last + 1;
		let mangled_var = !last in
		let new_varMap = 
          (match (Map.add varMap ~key:var ~data: mangled_var) with
	         | `Ok new_varMap -> new_varMap
             | `Duplicate -> raise(Failure("shadowing variable"))) in
		
		(* get the mangled name corresponding to this polarity *)
		let (chan, sign) = (match chanVar with
			      	          | Plus chan -> (chan, 1)
					          | Minus chan -> (chan, -1)) in
		let mangled_chan = 
		  (match Map.find varMap chan with
		     | None -> raise (Failure("not in scope"))
             | Some mangled_chan -> mangled_chan) * sign in

		(* receive deferred value and add it in the global map *)
		let var_value = (Chan.recvPi !globalMap mangled_chan) in
		globalMap := (match (Map.add !globalMap ~key:mangled_var ~data:var_value) with
	            		| `Ok new_globalMap -> new_globalMap
    		            | `Duplicate -> raise(Failure("shadowing variable")));
		new_varMap
  | _ -> (* TODO *) raise(Failure("language construct not implemented"))

and igneval = fun x -> ignore(eval x)

let main () =
  (* TODO: build parser that makes ASTs *)
  
  (*let ast = New ("c", Compose (Send (Plus "c", Str "Hello"),
                               Dot (Recv (Minus "c", "x"), Print (Var "x")))) in *)

  let ast = New ("z", Compose (New ("x", Compose (Send (Minus "x", Plus "z"),
                                                  Dot (Recv (Plus "x", "y"), 
                                                       Dot (Send (Minus "y", Plus "x"),
										     		        Dot (Recv (Plus "x", "t"),
													             Print (Var "t")
                                                                )
                                                           )
                                                      )
                                                 )
                                   ),
                               Dot (Recv (Plus "z", "v"),
 						            Send (Minus "v", Str "Hello")
						           )
                              )
				) in

  ignore(eval ((Map.empty (module String)),  (* empty local varMap *)
               ref (Map.empty (module Int)), (* ref to empty globalMap *)
               ref 0,                        (* ref to last used mangled name *)
               ast))                         (* ast *)

let () =
  main ();
  never_returns (Scheduler.go ())

