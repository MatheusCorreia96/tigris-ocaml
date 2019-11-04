// parser.mly

%{
  open Absyn
%}

%token <bool>          LOGIC
%token <int>           INTEGER
%token <string>        STRING
%token <float>         REAL
%token <Symbol.symbol> ID
%token                 IF THEN ELSE
%token                 WHILE DO BREAK
%token                 LET IN END
%token                 VAR FUNCTION TYPE
%token                 LPAREN "(" RPAREN ")"
%token                 COLON ":" COMMA "," SEMI ";"
%token                 PLUS "+" MINUS "-" TIMES "*" DIV "/" MOD "%" POW "^"
%token                 EQ "=" NE "<>"
%token                 LT "<" LE "<=" GT ">" GE ">="
%token                 AND "&" OR "|"
%token                 ASSIGN ":="
%token                 EOF


%right THEN ELSE DO IN
%nonassoc ASSIGN
%left OR
%left AND
%nonassoc EQ NE GT GE LT LE
%left PLUS MINUS
%left TIMES DIV MOD
%right POW


%start <Absyn.lexp> program



%%




program:
 | x=exp EOF {x}

exp:
 | x=LOGIC              {$loc, BoolExp x}
 | x=INTEGER            {$loc, IntExp x}
 | WHILE t=exp DO b=exp {$loc, WhileExp (t, b)}
 | LET d=decs IN e=exp  {$loc, LetExp (d, e)}
 | v=var                {$loc, VarExp v}
 | MINUS a=exp          {$loc, OpExp (MinusOp, ($loc, IntExp(0)), a)}
 | a=exp PLUS b=exp     {$loc, OpExp (PlusOp, a, b)}  
 | a=exp MINUS b=exp    {$loc, OpExp (MinusOp, a, b)} 
 | a=exp TIMES b=exp    {$loc, OpExp (TimesOp, a, b)}
 | a=exp DIV b=exp      {$loc, OpExp (DivOp, a, b)}
 | a=exp MOD b=exp      {$loc, OpExp (ModOp, a, b)}
 | a=exp POW b=exp      {$loc, OpExp (PowOp, a, b)} 
 | a=exp EQ b=exp       {$loc, OpExp (EqOp, a, b)}  
 | a=exp NE b=exp       {$loc, OpExp (NeOp, a ,b)}
 | a=exp GT b=exp       {$loc, OpExp (GtOp, a, b)}
 | a=exp GE b=exp       {$loc, OpExp (GeOp, a, b)}
 | a=exp LT b=exp       {$loc, OpExp (LtOp, a, b)}
 | a=exp LE b=exp       {$loc, OpExp (LeOp, a, b)}
 | a=exp AND b=exp      {$loc, OpExp (AndOp, a, b)}
 | a=exp OR b=exp       {$loc, OpExp (OrOp, a, b)}
 | a=var ASSIGN b=exp   {$loc, AssignExp (a, b)}
 | a=ID LPAREN x=separated_list(COMMA, exp) RPAREN {$loc, CallExp (a, x)}
 | IF a=exp THEN b=exp ELSE c=exp {$loc, IfExp (a, b, Some(c))}
 | IF a=exp THEN b=exp  {$loc, IfExp (a, b, None)}
 | BREAK                {$loc, BreakExp}
 | LPAREN exps=separated_list(SEMI,exp) RPAREN  {$loc, SeqExp (exps)}

decs:
 | l=list(dec) {l}

dec:
 | v=vardec {v}
 | a=nonempty_list(typedec) {$loc, MutualTypeDecs a}
 | a=nonempty_list(funcdec) {$loc, MutualFunctionDecs a}

typedec:
 | TYPE a=ID ASSIGN b=typeCons {$loc, (a,b)}

typeCons:
 | a=ID {$loc, NameCons(a)}

funcdec:
 | FUNCTION a=ID LPAREN b=separated_list(COMMA,param) RPAREN EQ c=exp {$loc, (a, b, None, c)}
 | FUNCTION a=ID LPAREN b=separated_list(COMMA,param) RPAREN COLON c=ID EQ d=exp {$loc, (a,b, Some($loc(c),c), d)}

vardec:
 | VAR v=ID ":" t=ID ":=" e=exp {$loc, VarDec (v, Some ($loc(t), t), e)}
 | VAR v=ID ":=" e=exp {$loc, VarDec (v, None, e)}

var:
 | x=ID {$loc, SimpleVar x}

param:
 | a=ID COLON b=ID {$loc, (a,b)}



