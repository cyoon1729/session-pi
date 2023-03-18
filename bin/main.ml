open Core
open SessionPi
open Async
open Pi

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
	  | Var var -> let mangled_var = (match Map.find varMap var with
             	   					    | None -> raise (Failure("bad name"))
		                		 	    | Some v -> v) in
				   let v = (match Map.find !globalMap mangled_var with
					          | None -> raise (Failure("bad name"))
             				  | Some v -> v) in
				   v >>> 
				   (fun v -> (match v with
							  	| Strg s -> printf "%s\n" s
							  	| _ -> raise(Failure("TODO")))
				   )
	  | _ -> raise(Failure("TODO"))
	  );
	  varMap
  | Compose (p, q) ->
		let p1 = spawn_thread igneval (varMap, globalMap, last, p) in
		let p2 = spawn_thread igneval (varMap, globalMap, last, q) in
		Core_thread.join p1;
		Core_thread.join p2;
		varMap
  | Dot (p, q) ->
		let new_varMap = eval (varMap, globalMap, last, p) in
		eval (new_varMap, globalMap, last, q)
  | New (chan, p) ->
		last := !last + 1;
		let mangled_chan = !last in
		let new_varMap = 
  		  (match (Map.add varMap ~key:chan ~data:mangled_chan) with
	         | `Ok new_varMap -> new_varMap
             | `Duplicate -> raise(Failure("shadowing name"))) in
		globalMap := (Chan.newPiChan !globalMap mangled_chan);
		eval (new_varMap, globalMap, last, p);
  | Send (chanVar, e) ->
		let (chan, sign) = (match chanVar with
			      	          | Plus chan -> (chan, 1)
					          | Minus chan -> (chan, 0 - 1)) in
		let mangled_chan = 
          (match Map.find varMap chan with
             | None -> raise (Failure("not in scope"))
             | Some mangled_chan -> mangled_chan) * sign in
		(match e with
		   | Str s -> (Chan.sendPi !globalMap mangled_chan (Strg s))
		   | _ -> raise (Failure("Send not implemented for this type")));
		varMap
  | Recv (chanVar, var) ->
		last := !last + 1;
		let mangled_var = !last in
		let new_varMap = 
          (match (Map.add varMap ~key:var ~data: mangled_var) with
	         | `Ok new_varMap -> new_varMap
             | `Duplicate -> raise(Failure("shadowing variable"))) in
		
		let (chan, sign) = (match chanVar with
			      	          | Plus chan -> (chan, 1)
					          | Minus chan -> (chan, 0 - 1)) in
		let mangled_chan = 
		  (match Map.find varMap chan with
		     | None -> raise (Failure("not in scope"))
             | Some mangled_chan -> mangled_chan) * sign in
		let var_value = (Chan.recvPi !globalMap mangled_chan) in
		globalMap := (match (Map.add !globalMap ~key:mangled_var ~data:var_value) with
	            		| `Ok new_globalMap -> new_globalMap
    		            | `Duplicate -> raise(Failure("shadowing variable")));
		new_varMap
  | _ -> raise(Failure("language construct not implemented"))

and igneval (varMap, globalMap, last, ast) = 
  ignore(eval (varMap, globalMap, last, ast));
  ()

let main () =
  let ast = New ("c", (Compose ((Send ((Plus "c"), (Str "Hello"))),
                                (Dot ((Recv ((Minus "c"), "x")), (Print (Var "x"))))
                               )
                      )
                ) in
  ignore(eval ((Map.empty (module String)), ref (Map.empty (module Int)), ref 0, ast));
  ()

let () =
  main ();
  never_returns (Scheduler.go ())

