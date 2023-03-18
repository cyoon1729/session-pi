open Async

type name = string

type chan = string

type chanVar = Plus of string
		     | Minus of string

type typ = 
  | LitInt
  | LitBool
  | LitChar
  | LitString
  | TChan of typ
  | TChanTup of typ list
  | SessTyp
and sessTyp = 
  | End
  | Send of typ * sessTyp
  | Recv of typ * sessTyp
  | Select of (typ * sessTyp) list
  | Offer of (typ * sessTyp) list

type pattern = 
  | PatVar of name
  | PatTup of pattern list
  | Wildcard

type expr = (* sth that can be sent via channels *)
  | Str of string
  | Var of string
  | Plus of string
  | Minus of string

type pi = 
  | Nil
  | Print of expr    
  | Compose of pi * pi  (* P | Q *)
  | Dot of pi * pi      (* P.Q *)
  | New of chan * pi    (* (new x) P *)
  | Send of chanVar * expr
  | Recv of chanVar * string
  | Select of name * pi
  | Offer of (name * pi) list

(* the value of a var *)
type value = Strg of string | PiChan of value Pipe.Reader.t * value Pipe.Writer.t

type piChan = {port1 : chan; port2 : chan;}
