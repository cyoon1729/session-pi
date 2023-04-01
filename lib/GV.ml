type identifier =
  | ChanEndpoint of string
  | Variable of string

type constant =
  | Fix
  | Fork
  | Request of int (* int here is the buffer size *)
  | Accept of int (* int here is the buffer size *)
  | Send
  | Receive

type value =
  | Identifier of identifier
  | Constant of constant
  | Lambda of identifier * expr
  | Tup of value * value
  | Integer of int
  | String of string
  | Bool of bool
  | Unit

and data =
  | Value
  | Label of string

and expr =
  | EValue of value
  | EApp of expr * expr
  | ETup of expr * expr
  | ELetTup of identifier * identifier * expr * expr
  | ESelect of data * expr
  | ECase of expr * (identifier * expr) list

and configuration =
  | CExpr of expr
  (* CBufferEndpoint: endpoint c -> (peer endpoint d, buffer size, buffer) *)
  | CBufferEndpoint of identifier * identifier * int * data list
  | CPar of configuration * configuration
  (* CNewChan: endpoint c -> (peer endpoint d, configuration) *)
  | CNewChan of identifier * identifier * configuration

and evalContext =
  (* Eval Context is an expression with a hole *)
  | Hole
  | HApp of evalContext * expr
  | HValueApp of value * evalContext
  | HTup of evalContext * expr
  | HValueTup of value * evalContext
  | HLetTup of identifier * identifier * evalContext * expr
  | HSelect of identifier * evalContext
  | HCase of (identifier * expr) list
