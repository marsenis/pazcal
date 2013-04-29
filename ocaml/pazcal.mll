{
   open Printf

   let linecount = ref 1

   type tokens = 
          T_bool | T_break | T_case | T_char
        | T_const | T_continue | T_default | T_do
        | T_DOWNTO | T_else | T_false | T_FOR
        | T_FORM | T_FUNC | T_if | T_int
        | T_NEXT | T_PROC | T_PROGRAM
        | T_REAL | T_return | T_STEP | T_switch
        | T_TO | T_true | T_while | T_WRITE
        | T_WRITELN | T_WRITESP | T_WRITESPLN | T_id
        | T_int_const
        | T_float_const | T_char_const | T_string_literal
        | T_MOD | T_not | T_eof

        | T_gr | T_le | T_plus | T_minus
        | T_mult | T_divide | T_mod
        | T_assign | T_ref | T_semicolon | T_dot
        | T_lparen | T_rparen | T_colon | T_comma
        | T_lbracket | T_rbracket | T_lcbracket | T_rcbracket
        | T_eq | T_neq | T_geq | T_leq
        | T_and | T_or | T_plusplus | T_minusminus
        | T_pluseq | T_minuseq | T_multeq | T_diveq
        | T_modeq
}

let d = ['0' - '9' ]
let l = [ 'a'- 'z' 'A' - 'Z' ]
let alph = [ '0' - '9' 'a' - 'z' 'A' - 'Z' ]
let w = [ ' ' '\t' '\n' '\r' ]
let printable = [ ' ' - '~' ]
let escapeseq = "\\n" | "\\t" | "\\r" | "\\0" | "\\\\" | "\\\'" | "\\\""

rule lexer = parse
   | "and"           { T_and }
   | "bool"          { T_bool }
   | "break"         { T_break }
   | "case"          { T_case }
   | "char"          { T_char }
   | "const"         { T_const }
   | "continue"      { T_continue }
   | "default"       { T_default }
   | "do"            { T_do }
   | "DOWNTO"        { T_DOWNTO }
   | "else"          { T_else }
   | "false"         { T_false }
   | "FOR"           { T_FOR }
   | "FORM"          { T_FORM }
   | "FUNC"          { T_FUNC }
   | "if"            { T_if }
   | "int"           { T_int }
   | "MOD"           { T_MOD }
   | "NEXT"          { T_NEXT }
   | "not"           { T_not }
   | "or"            { T_or }
   | "PROC"          { T_PROC }
   | "PROGRAM"       { T_PROGRAM }
   | "REAL"          { T_REAL }
   | "return"        { T_return }
   | "STEP"          { T_STEP }
   | "switch"        { T_switch }
   | "TO"            { T_TO }
   | "true"          { T_true }
   | "while"         { T_while }
   | "WRITE"         { T_WRITE }
   | "WRITELN"       { T_WRITELN }
   | "WRITESP"       { T_WRITESP }
   | "WRITESPLN"     { T_WRITESPLN }

   | l ( alph | '_' )* { T_id }
   | ('0' | [ '1' - '9'] d* ) { T_int_const }
   | d+ '.' d+ ( ('e'|'E')('+'|'-')? d+ )? { T_float_const }

   | '>'  { T_gr }
   | '<'  { T_le }
   | '+'  { T_plus }
   | '-'  { T_minus }
   | '*'  { T_mult }
   | '/'  { T_divide }
   | '%'  { T_mod }
   | '!'  { T_not }
   | '='  { T_assign }
   | '&'  { T_ref }
   | ';'  { T_semicolon }
   | '.'  { T_dot }
   | '('  { T_lparen } 
   | ')'  { T_rparen }
   | ':'  { T_colon }
   | ','  { T_comma }
   | '['  { T_lbracket }
   | ']'  { T_rbracket }
   | '{'  { T_lcbracket }
   | '}'  { T_rcbracket }
   | "==" { T_eq }
   | "!=" { T_neq }
   | ">=" { T_geq }
   | "<=" { T_leq }
   | "&&" { T_and }
   | "||" { T_or }
   | "++" { T_plusplus }
   | "--" { T_minusminus }
   | "+=" { T_pluseq }
   | "-=" { T_minuseq }
   | "*=" { T_multeq }
   | "/=" { T_diveq }
   | "%=" { T_modeq }

   | [' ' '\t']      { lexer lexbuf }
   | "//" _*         { lexer lexbuf }

   | '\'' ( printable | escapeseq ) '\'' { T_char_const }
   | '"' ( [^ '\\' '"' '\n' ] | escapeseq )* '"' { T_string_literal }
   
   | "/*"  { comment lexbuf }

   | '\r'  { lexer lexbuf }
   | '\n'  { linecount := !linecount + 1; lexer lexbuf }

   | _ as ch         { printf "Unrecognized character '%c'\n" ch; lexer lexbuf }

   | eof             { T_eof }

   and comment = parse
     "*/" { lexer lexbuf }
   | '\n' { linecount := !linecount + 1; comment lexbuf }
   | '*'  { comment lexbuf }
   | [^ '*' '\n']+ { comment lexbuf }


{
   (* For Debugging *)
   let printToken t =
      match t with
           T_assign -> "T_assign"
         | T_and -> "T_and"
         | T_bool -> "T_bool"
         | T_break -> "T_break"
         | T_case -> "T_case"
         | T_char -> "T_char"
         | T_const -> "T_const"
         | T_continue -> "T_continue"
         | T_default -> "T_default"
         | T_do -> "T_do"
         | T_DOWNTO -> "T_DOWNTO"
         | T_else -> "T_else"
         | T_false -> "T_false"
         | T_float_const -> "T_float_const"
         | T_FOR -> "T_FOR"
         | T_FORM -> "T_FORM"
         | T_FUNC -> "T_FUNC"
         | T_if -> "T_if"
         | T_int -> "T_int"
         | T_id -> "T_id"
         | T_int_const -> "T_int_const"
         | T_MOD -> "T_MOD"
         | T_NEXT -> "T_NEXT"
         | T_or -> "T_or"
         | T_PROC -> "T_PROC"
         | T_PROGRAM -> "T_PROGRAM"
         | T_REAL -> "T_REAL"
         | T_return -> "T_return"
         | T_STEP -> "T_STEP"
         | T_switch -> "T_switch"
         | T_TO -> "T_TO"
         | T_true -> "T_true"
         | T_while -> "T_while"
         | T_WRITE -> "T_WRITE"
         | T_WRITELN -> "T_WRITELN"
         | T_WRITESP -> "T_WRITESP"
         | T_WRITESPLN -> "T_WRITESPLN"
         | T_gr -> "T_gr"
         | T_le -> "T_le"
         | T_plus -> "T_plus"
         | T_minus -> "T_minus"
         | T_mult -> "T_mult"
         | T_divide -> "T_divide"
         | T_mod -> "T_mod"
         | T_not -> "T_not"
         | T_eq -> "T_eq"
         | T_ref -> "T_ref"
         | T_semicolon -> "T_semicolon"
         | T_dot -> "T_dot"
         | T_lparen  -> "T_lparen "
         | T_rparen -> "T_rparen"
         | T_colon -> "T_colon"
         | T_comma -> "T_comma"
         | T_lbracket -> "T_lbracket"
         | T_rbracket -> "T_rbracket"
         | T_lcbracket -> "T_lcbracket"
         | T_rcbracket -> "T_rcbracket"
         | T_neq -> "T_neq"
         | T_geq -> "T_geq"
         | T_leq -> "T_leq"
         | T_plusplus -> "T_plusplus"
         | T_minusminus -> "T_minusminus"
         | T_pluseq -> "T_pluseq"
         | T_minuseq -> "T_minuseq"
         | T_multeq -> "T_multeq"
         | T_diveq -> "T_diveq"
         | T_modeq -> "T_modeq"
         | T_string_literal -> "T_string_literal"
         | T_char_const -> "T_char_const"
         | T_eof -> "T_eof"

   let rec lexInput lexbuf =
      let t = lexer lexbuf
      in
         if t = T_eof then ()
         else ( printf ":%d: %s\n" !linecount (printToken t); lexInput lexbuf )

   let main () =
      let cin =
         if Array.length Sys.argv > 1 then
            open_in Sys.argv.(1)
         else stdin
      in
         let lexbuf = Lexing.from_channel cin in
            lexInput lexbuf

   let _ = Printexc.print main ()
}
