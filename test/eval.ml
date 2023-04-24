open Core
open SessionPi.Pi
open SessionPi.SyncEval
open SessionPi.Print

let comm1 = 
    Par
      ( PInput ("c", [ "x", Int ], POutput ("c", [ DataInt 0 ], PEnd))
      , POutput ("c", [ DataInt 1 ], PInput ("c", [ "z", Int ], PEnd)) )

let comm2 = 
    Par
      ( POutput ("c", [ DataInt 1 ], PInput ("c", [ "z", Int ], PEnd))
      , PInput ("c", [ "x", Int ], POutput ("c", [ DataInt 0 ], PEnd))
      )

let branch1 = 
    Par
      ( PBranch ("c", [ "lab1", POutput ("d", [ DataInt 1 ], PEnd); "lab2", PEnd ])
      , PChoice ("c", "lab1", PInput ("d", [ "x", Int ], PEnd)) )

let branch2 = 
    Par
      ( PChoice ("c", "lab1", PInput ("d", [ "x", Int ], PEnd))
      , PBranch ("c", [ "lab1", POutput ("d", [ DataInt 1 ], PEnd); "lab2", PEnd ])
      )

let rep1 = 
    Par
      ( POutput ("c", [ DataInt 1 ], PInput ("c", [ "z", Int ], PEnd))
      , Rep (PInput ("c", [ "x", Int ], POutput ("c", [ DataInt 0 ], PEnd)))
      )

let rep2 = 
    Par
      ( Rep (PChoice ("c", "lab1", PInput ("d", [ "x", Int ], PEnd)))
      , PBranch ("c", [ "lab1", POutput ("d", [ DataInt 1 ], PEnd); "lab2", PEnd ])
    )

let new1 = 
    New ("c", Int, 
      Par
        ( PInput ("c", [ "x", Int ], POutput ("c", [ DataInt 0 ], PEnd))
        , POutput ("c", [ DataInt 1 ], PInput ("c", [ "z", Int ], PEnd)) )
    )

let debugEval p = 
  print_endline ("Process: " ^ printProcess p);
  ignore (debugReduce [ p ]);
  print_endline "\n"
