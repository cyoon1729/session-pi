(* p. 197 *)
type label = string
type typeVar = string

type sType =
  | STypeVar of typeVar
  | SEnd
  | SInput of tType list * sType
  | SOutput of tType list * sType
  | SBranch of (label * sType) list
  | SChoice of (label * sType) list
  | SMu of typeVar * sType

and tType =
  | TTypeVar of typeVar
  | SType of sType
  | NChan of tType list
  | TMu of typeVar * tType

(* p. 198 *)

type name = string

type process =
  | PEnd
  | Par of process * process
  | Rep of process
  | PInput of name * (name * tType) list * process
  | POutput of name * name list * process
  | New of name * tType * process
  | PBranch of name * (label * process) list
  | PChoice of name * label * process
