{
  open Parser
}

rule tokenize = parse
| [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| "<|"   { LEFTTRI }
| "|>"   { RIGHTTRI }
| "|"    { BAR }
| "."    { DOT }
| "!"    { REP }
| "?"    { ASK }
| "&"    { BRANCH }
| "+"    { CHOICE }
| '('    { LPAREN }
| ')'    { RPAREN }
| '['    { LSQUARE }
| ']'    { RSQUARE }
| '{'    { LBRACKET }
| '}'    { RBRACKET }
| '<'    { LSBRACKET }
| '>'    { RSBRACKET }
| ","    { COMMA }
| ":"    { COLON }
| "^"    { HAT }
| "`"    { TICK }
| "m"    { MU }
| "ν"    { NU }
| "zero" { ZERO }
| "end"  { END }
| ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]* as id { NAME (id) }
| ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]* as id { TYPEVAR (id) }
| ['\''] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]* as id { LABEL (id) }
| eof { EOF }
