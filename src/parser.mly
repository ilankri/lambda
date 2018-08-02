%token EOF LPAREN RPAREN COMMA COLON PLUS STAR DASH ARROW BIG_ARROW EQ
%token FST SND FIX IF THEN ELSE FUN LET IN INT BOOL
%token<int> C_INT
%token<bool> C_BOOL
%token<string> ID

%nonassoc IN ELSE
%right BIG_ARROW
%left PLUS DASH
%right ARROW
%left STAR
%nonassoc FST SND FIX

%start<Lambda.expr> prog

%%

prog:
  | e = expr EOF { e }

expr:
  | e = simple_expr { e }
  | op = unop e = expr { Lambda.operator op e }
  | e1 = expr op = binop e2 = expr { Lambda.binop op e1 e2 }
  | LPAREN e1 = expr COMMA e2 = expr RPAREN { Lambda.Pair (e1, e2) }
  | e1 = simple_expr e2 = simple_expr  { Lambda.App (e1, e2) }
  | FUN x = id t = type_annot? BIG_ARROW e = expr
    { Lambda.Abstr (x, t, e) }
  | LET x = id t = type_annot? EQ e1 = expr IN e2 = expr
    { Lambda.Let (x, t, e1, e2) }
  | IF c = expr THEN e1 = expr ELSE e2 = expr { Lambda.if_then_else c e1 e2 }

simple_expr:
  | x = id { Lambda.Var x }
  | c = const { Lambda.Cst c }
  | LPAREN e = expr RPAREN { e }

ty:
  | INT { Type.Int }
  | BOOL { Type.Bool }
  | x = ID { Type.Var (Var.make x) }
  | t = ty ARROW u = ty { Type.Arrow (t, u) }
  | t = ty STAR u = ty { Type.Prod (t, u) }

%inline type_annot:
  | t = preceded(COLON, ty) { t }

%inline id:
  | x = ID { Lambda.Id x }

%inline const:
  | b = C_BOOL { Lambda.C_bool b }
  | i = C_INT { Lambda.C_int i }

%inline unop:
  | FST { Lambda.Fst }
  | SND { Lambda.Snd }
  | FIX { Lambda.Fix }

%inline binop:
  | PLUS { Lambda.Add }
  | STAR { Lambda.Mult }
  | DASH { Lambda.Sub }
