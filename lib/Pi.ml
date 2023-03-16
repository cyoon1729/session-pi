type name = string

type chan = string

type typ = 
  | LitInt of int
  | LitBool of bool
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

type pi = 
  | Nil
  | Compose of pi * pi
  | New of chan * typ * pi
  | Send of chan * typ * pi
  | Recv of chan * typ * pi
  | Select of name * pi
  | Offer of (name * pi) list

type piChan = {cTyp: typ; port1 : chan; port2 : chan;}
