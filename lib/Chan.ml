open Pi
open Core
open Async

module M = Map.M(String)

let newPiChan (cName : name) (cTyp : typ) chanMap =
    let (port1, port2) = (cName ^ "1", cName ^ "2") in
    let (r1, w1) = Pipe.create () in
    let (r2, w2) = Pipe.create () in
    let chan = {cTyp = cTyp; port1 = port1; port2 = port2;} in
    let newMap = List.fold_left
                  [(port1, (r1, w2)); (port2, (r2, w1))] 
                  (fun m (k, v) -> Map.add m ~key:k ~data:v)
                  chanMap 
    in (chan, newMap)

