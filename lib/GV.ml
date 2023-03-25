
type Identifier =
  | ChanEndpoint of string
  | Variable of string

type Constant = 
  | Fix
  | Fork
  | Request of int   (* int here is the buffer size *) 
  | Accept of int    (* int here is the buffer size *) 
  | Send
  | Receive

type Value = 
  | Identifier
  | Constant
  | Lambda of Variable * Expr
  | Tup of Value * Value 
  | Integer of int
  | String of string
  | Bool of bool
  | Unit

type Data =
  | Value
  | Label of string

type Expr = 
  | EValue
  | EApp of Expr * Expr
  | ETup of Expr * Expr
  | ELetTup of Variable * Variable * Expr * Expr
  | ESelect of Label * Expr
  | ECase of Expr * ((Label * Expr) list)

type Configuration = 
  | CAccessPoint of Expr
  (* CBufferEndpoint: endpoint c -> (peer endpoint d, buffer size, buffer) *)
  | CBufferEndpoint of ChanEndpoint * ChanEndpoint * int * (Data list)
  | CUnion of Configuration * Configuration
  (* CNewChan: endpoint c -> (peer endpoint d, configuration) *)
  | CNewChan of ChanEndpoint * ChanEndpoint * Configuration

(* Eval Context is an expression with a hole *)
type EvalContext = 
  | Hole
  | HApp of EvalContext * Expr
  | HValueApp of Value * EvalContext 
  | HTup of EvalContext * Expr 
  | HValueTup of Value * EvalContext
  | HLetTup of Variable * Variable * EvalContext * Expr
  | HSelect of Lable * EvalContext
  | HCase of EvalContext of ((Lable * Expr) list)
