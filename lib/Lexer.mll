{
  open Parser
}

rule tokenize = parse
| [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| "ν" { NU }
| "<|" { LEFTTRI }
| "|>" { RIGHTTRI }
| "|" { BAR }
| "/" { SLASH }
| "!"   { EMIT }
| "?"   { ASK }
| "print"   { PRINT }
| "Nil" { NIL }
| "true"  { TRUE }
| "false" { FALSE }
| ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]* as id { VARIABLE(id) }
| ['0'-'9'] ['0'-'9']* as lit { NUMBER (int_of_string lit) }
| '('   { LPAREN }
| ')'   { RPAREN }
| '<'   { LSBRACKET }
| '>'   { RSBRACKET }
| '{'   { RBRACKET }
| '}'   { LBRACKET }
| "."   { DOT }
| ","   { COMMA }
| "+"   { PLUS }
| "-"   { MINUS }
| ":"   { COLON }
| eof { EOF }
