%{
   open Pi
   open List
%}

%left BAR DOT

%start pi
%type <Pi.pi> pi
%type <Pi.expr> expr
%token <int> NUMBER
%token <string> VARIABLE
%token <string> STRING

%token LPAREN RPAREN LSBRACKET RSBRACKET LBRACKET RBRACKET
%token BAR DOT NU EMIT ASK NIL PRINT SLASH DQUOT
%token LEFTTRI RIGHTTRI
%token TRUE FALSE PLUS MINUS QUOT COLON COMMA EOF

%%

pi:
| compAtom									  { $1 }
| compAtom BAR pi                             { Compose($1, $3) }

compAtom:
| atom                                        { $1 }
| atom DOT compAtom                           { Seq($1, $3) }
| LPAREN NU VARIABLE RPAREN compAtom          { New($3, $5) }

atom:
| NIL                                         { Nil }
| PRINT expr                                  { Print($2) }
| LPAREN pi RPAREN                            { $2 }
| chan EMIT LSBRACKET expr RSBRACKET          { Send($1, $4) }
| chan ASK LPAREN VARIABLE RPAREN             { Recv($1, $4) }
| chan LEFTTRI VARIABLE                       { Select($1, $3) }
| chan RIGHTTRI LBRACKET labelledPis RBRACKET { Offer($1, $4) }

expr:
| NUMBER              { Num($1) }
| TRUE                { Bool(true) }
| FALSE               { Bool(false) }
| DQUOT VARIABLE DQUOT       { Str($2) }
| VARIABLE            { Var($1) }
| chan                { ChanVar($1) }

chan:
| VARIABLE PLUS  { Plus($1) }
| VARIABLE MINUS { Minus($1) }

labelledPi:
| VARIABLE COLON pi { Branch($1, $3) }

labelledPis:
|                              { [] }
| labelledPi                   { [$1] }
| labelledPi COMMA labelledPis { ($1)::($3) }
