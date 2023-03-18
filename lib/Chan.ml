open Core
open Async
open Pi

let newPiChan (chanMap : globalMapType) (cName : int) : globalMapType =
  let r1, w1 = Pipe.create () in
  (* create one half-duplex pipe *)
  let r2, w2 = Pipe.create () in
  (* create another half-duplex pipe *)
  (* each channel has two polarities; 
		the + one corresponds to the positive mangled value
		the - one corresponds to the negative mangled value *)
  let newMap =
    List.fold
      [ cName, Deferred.return (PiChan (r1, w2, r2, w1))
      ; (* add + end *)
        -cName, Deferred.return (PiChan (r2, w1, r1, w2))
      ]
      (* add - end *)
      ~init:chanMap
      ~f:(fun m (k, v) ->
        match Map.add m ~key:k ~data:v with
        | `Ok nm -> nm
        | `Duplicate -> m)
  in
  newMap
;;

let getChanByName (chanMap : globalMapType) (pName : int)
  : (value Pipe.Reader.t
    * value Pipe.Writer.t
    * value Pipe.Reader.t
    * value Pipe.Writer.t)
  Deferred.t
  =
  (* get the ***deferred*** value associated with this mangled name *)
  let v =
    match Map.find chanMap pName with
    | None -> raise (Failure ("not in scope: " ^ string_of_int pName))
    | Some v -> v
  in
  (* "peel off" a layer of types and put it back in the Deferred monad *)
  v
  >>| function
      | PiChan (x, y, z, t) -> x, y, z, t
      | _ -> raise (Failure "not a channel")
;;

let closePi (chanMap : globalMapType) (pName : int) : unit =
  getChanByName chanMap pName
  >>> fun (r, w, _, _) ->
    Pipe.close w;
    Pipe.close_read r

let sendPi (chanMap : globalMapType) (pName : int) (data : 'a) : unit =
  getChanByName chanMap pName
  >>> (* the actual value of the channel is deferred *)
  fun (_, w, _, _) ->
  Pipe.write w data
  >>> (* block until data fits into pipe *)
  fun () ->
  ()
;;

let recvPi (chanMap : globalMapType) (pName : int) : 'a Deferred.t =
  getChanByName chanMap pName
  >>= (* the actual value of the channel is deferred *)
  fun (r, _, _, _) ->
  Pipe.read r
  >>| function
      | `Eof -> failwith "EOF"
      | `Ok data -> data
;;
