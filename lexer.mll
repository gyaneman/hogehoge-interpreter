{
open Parser
exception Eof
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = digit | alpha | '\''


rule token = parse
  | space+  { token lexbuf }
  | digit+ as lxm { INT (int_of_string lxm) }
  | ','     { CAMMA }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { TIMES }
  | '/'     { DIV }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | '='     { EQ }
  | "true"  { TRUE }
  | "false" { FALSE }
  | "let"   { LET }
  | "in"    { IN }
  | "proc"  { PROC }
  | "letrec"{ LETREC }
  | "if"    { IF }
  | "then"  { THEN }
  | "else"  { ELSE }
  | alpha alnum*  as str { IDENTIFIER (str) }
  | eof     { EOF }
  | _
    {
      let message = Printf.sprintf
        "unknown token %s near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)
      in
      failwith message
    }
