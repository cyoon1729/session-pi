open Core
open Async

open Pi

let newPiChan chanMap (cName : name) (cTyp : typ) =
  let (port1, port2) = (cName ^ "1", cName ^ "2") in
  let (r1, w1) = Pipe.create () in
  let (r2, w2) = Pipe.create () in
  let chan = {cTyp = cTyp; port1 = port1; port2 = port2;} in
  let newMap = List.fold
                 [(port1, (r1, w2)); (port2, (r2, w1))]
                 ~init:chanMap 
                 ~f:(fun m (k, v) ->
                       match Map.add m ~key:k ~data:v with
                       | `Ok nm -> nm
                       | `Duplicate -> m)
  in (chan, newMap)

let getChanByName chanMap (pName : name) = 
  match Map.find chanMap pName with
  | None -> raise (Failure("Channel " ^ pName ^ " not found"))
  | Some v -> v

let closePi chanMap (pName : name) =
  let (r, w) = getChanByName chanMap pName
  in
    Pipe.close w;
    Pipe.close_read r

let sendPi chanMap (pName : name) data : unit =
  let (_, w) = getChanByName chanMap pName
  in
  Pipe.write w data >>> (* block until data fits into pipe *)
  fun () -> ()

let recvPi chanMap (pName : name) = 
  let (r, _) = getChanByName chanMap pName
  in
  Pipe.read r >>|
  function
  | `Eof -> failwith "EOF"
  | `Ok data -> data 
