{
exception Invalid_integer_constant
}

let newline = '\n' | '\r' | "\r\n"

let blank = ' ' | '\t'

let digit = ['0'-'9']

let lowercase_letter = ['a'-'z']

let uppercase_letter = ['A'-'Z']

let letter = uppercase_letter | lowercase_letter

let alphanum = letter | digit | '_'

rule token = parse
  | newline
    {
      Lexing.new_line lexbuf;
      token lexbuf
    }
  | blank+ { token lexbuf }
  | eof { Parser.EOF }
  | "(" { Parser.LPAREN }
  | ")" { Parser.RPAREN }
  | "," { Parser.COMMA }
  | ":" { Parser.COLON }
  | "+" { Parser.PLUS }
  | "*" { Parser.STAR }
  | "-" { Parser.DASH }
  | "->" { Parser.ARROW }
  | "=>" { Parser.BIG_ARROW }
  | "=" { Parser.EQ }
  | "fst" { Parser.FST }
  | "snd" { Parser.SND }
  | "fix" { Parser.FIX }
  | "if" { Parser.IF }
  | "then" { Parser.THEN }
  | "else" { Parser.ELSE }
  | "fun" { Parser.FUN }
  | "let" { Parser.LET }
  | "in" { Parser.IN }
  | "int" { Parser.INT }
  | "bool" { Parser.BOOL }
  | digit+ as i
    {
      try Parser.C_INT (int_of_string i) with
      | Failure _ -> raise Invalid_integer_constant
    }
  | "false" { Parser.C_BOOL false }
  | "true" { Parser.C_BOOL true }
  | alphanum+ as id { Parser.ID id }
