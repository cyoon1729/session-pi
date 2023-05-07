%{
   open Pi
   open List
%}

%left REP
%left DOT

%start pi
%type <Pi.process> pi
%token <Pi.name> NAME
%token <Pi.label> LABEL
%token <Pi.typeVar> TYPEVAR
%token <int> INT

%token <string> VARIABLE
%token <string> STRING

%token LPAREN RPAREN LSQUARE RSQUARE LBRACKET RBRACKET
%token ZERO BAR DOT NU REP ASK LEFTTRI RIGHTTRI
%token COMMA COLON
%token END LSBRACKET RSBRACKET BRANCH CHOICE HAT TICK MU
%token EOF
%token INTTYPE

%%

sType:
| TYPEVAR                                      { STypeVar $1 }
| END                                          { SEnd }
| ASK LBRACKET tTypeList RBRACKET DOT sType    { SInput ($3, $6) }
| REP LBRACKET tTypeList RBRACKET DOT sType    { SOutput ($3, $6) }
| BRANCH LSBRACKET sTypeLabels RSBRACKET       { SBranch $3 }
| CHOICE LSBRACKET sTypeLabels RSBRACKET       { SChoice $3 }
| MU TYPEVAR DOT sType                         { SMu ($2, $4) }

sTypeLabels:
| LABEL COLON sType                            { [($1, $3)] }
| LABEL COLON sType COMMA sTypeLabels          { ($1, $3) :: $5 }

tType:
| INTTYPE                                      { Int }
| TYPEVAR                                      { TTypeVar $1 }
| TICK LSQUARE sType RSQUARE                   { SType $3 }
| HAT LSQUARE tTypeList RSQUARE                { NChan $3 }
| MU TYPEVAR DOT tType                         { TMu ($2, $4) }

tTypeList:
| tType                                        { [$1] }
| tType COMMA tTypeList                        { $1 :: $3 }

pi:
| atom   								 	   { $1 }
| atom BAR pi                                  { Par ($1, $3) }

atom:
| ZERO                                         { PEnd }
| REP atom                                     { Rep $2 }
| NAME ASK LSQUARE inputArgs RSQUARE DOT atom  { PInput ($1, $4, $7) }
| NAME REP LSQUARE outputArgs RSQUARE DOT atom { POutput ($1, $4, $7) }
| LPAREN NU NAME COLON tType RPAREN atom       { New ($3, $5, $7) }
| NAME RIGHTTRI LBRACKET branchArgs RBRACKET   { PBranch ($1, $4) }
| NAME LEFTTRI LABEL DOT atom                  { PChoice ($1, $3, $5) }
| LPAREN pi RPAREN                             { $2 }

inputArgs:
| NAME COLON tType                             { [($1, $3)] }
| NAME COLON tType COMMA inputArgs             { ($1, $3) :: $5 }

outputArgs:
| NAME                                         { [DataVar $1] }
| INT                                          { [DataInt $1] }
| NAME COMMA outputArgs                        { (DataVar $1) :: $3 }
| INT  COMMA outputArgs                        { (DataInt $1) :: $3 }

branchArgs:
| LABEL COLON pi                               { [($1, $3)] }
| LABEL COLON pi COMMA branchArgs              { ($1, $3) :: $5 }
