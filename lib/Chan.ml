open Core
open Async
open GV

let globalMapAdd (var : int) (value : mapValue Deferred.t) (globalMap : globalMapType)
  : globalMapType
  =
  match Map.add globalMap ~key:var ~data:value with
  | `Ok nm -> nm
  | `Duplicate -> raise (Failure "name shadowing")
;;

let newPiChan (globalMap : globalMapType) (cName : int) : globalMapType =
  (* create two half-duplex pipep *)
  let r1, w1 = Pipe.create () in
  let r2, w2 = Pipe.create () in
  (* each channel has two polarities; 
		the + one corresponds to the positive mangled value
		the - one corresponds to the negative mangled value *)
  globalMap
  |> globalMapAdd cName (Deferred.return (PiChan (r1, w2, r2, w1)))
  |> globalMapAdd (-cName) (Deferred.return (PiChan (r2, w1, r1, w2)))
;;

(*
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
  >>> fun (r1, w1, r2, w2) ->
  Pipe.close w1;
  Pipe.close w2;
  Pipe.close_read r1;
  Pipe.close_read r2
;;

let sendPi (chanMap : globalMapType) (pName : int) (data : 'a) : unit Deferred.t =
  getChanByName chanMap pName
  >>= (* the actual value of the channel is deferred *)
  fun (_, w, _, _) ->
  Pipe.write w data
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
*)
