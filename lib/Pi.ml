open Core
open Async

type name = string
type chan = string

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

type expr =
  (* sth that can be sent via channels *)
  | Str of string
  | Var of string
  | Plus of string
  | Minus of string

type chanVar =
  (* channel with polarity *)
  | Plus of string
  | Minus of string

type pi =
  (* process *)
  | Nil
  | Print of expr   
  | Compose of pi * pi       (* P | Q *)
  | Dot of pi * pi           (* P.Q *)
  | New of chan * pi         (* (new c) P *)
  | Send of chanVar * expr   (* c+/c-<x>. P *)
  | Recv of chanVar * string (* c+/c-(x). P *)
  | Select of name * pi		 
  | Offer of (name * pi) list

(* the value of a var/chan in the globalMap *)
type value =
  | Strg of string
  | PiChan of value Pipe.Reader.t * value Pipe.Writer.t

(* global map with deferred values *)
type globalMapType = (int, value Deferred.t, Int.comparator_witness) Map.t
