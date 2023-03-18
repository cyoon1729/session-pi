open Core
open Async
open Pi

let newPiChan chanMap (cName : int) =
  let (r1, w1) = Pipe.create () in
  let (r2, w2) = Pipe.create () in
  let newMap = List.fold
                 [(cName, Deferred.return (PiChan (r1, w2))); 
                  (0 - cName, Deferred.return (PiChan (r2, w1)))]
                 ~init:chanMap 
                 ~f:(fun m (k, v) ->
                       match Map.add m ~key:k ~data:v with
                       | `Ok nm -> nm
                       | `Duplicate -> m)
  in newMap

let getChanByName chanMap (pName : int) : (value Pipe.Reader.t * value Pipe.Writer.t) Deferred.t = 
  let v = (match Map.find chanMap pName with
             | None -> raise (Failure("not in scope"))
             | Some v -> v) in
  v >>|
  (fun v -> match v with
			 | PiChan (x, y) -> (x, y)
			 | _ -> raise(Failure("not a channel")))

(*
let closePi chanMap (pName : int) : unit =
  let (r, w) = getChanByName chanMap pName
  in
    Pipe.close w;
    Pipe.close_read r
*)

let sendPi chanMap (pName : int) data : unit =
  (getChanByName chanMap pName) >>>
  (fun (_, w) -> Pipe.write w data >>> (* block until data fits into pipe *)
  				fun () -> ())

let recvPi chanMap (pName : int) : 'a Deferred.t = 
  (getChanByName chanMap pName) >>=
  (fun (r, _) -> Pipe.read r >>|
				 function
				 | `Eof -> failwith "EOF"
				 | `Ok data -> data)
