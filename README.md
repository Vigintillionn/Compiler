# Grammar

<program>     ::= <statement> { <statement> }
<statement>   ::= <block>
              |   <var_decl>
              |   <assignment>
              |   <if_stmt>
              |   <loop_stmt>
              |   <func_decl>
              |   <return_stmt>
              |   <expr> ";"

<block>       ::= "{" { <statement> } "}"
<var_decl>    ::= "var" <identifier> [ ":" <type> ] "=" <expr> ";"
<assignment>  ::= <identifier> "=" <expr> ";"
<if_stmt>     ::= "if" "(" <expr> ")" <block> [ "else" <block> ]
<loop_stmt>   ::= "loop" <block>
              |   "while" "(" <expr> ")" <block>
              |   "for" "(" <expr> ";" <expr> ";" <expr> ")" <block>
<func_decl>   ::= "proc" <identifier> "(" <param_list> ")" [ "->" <type> ] <block>
<param_list>  ::= <param> | <param> "," <param_list>
<param>       ::= <identifier> ":" <type>
<return_stmt> ::= "ret" <expr> ";"
<expr>        ::= <literal>
              |   <identifier>
              |   <binary_expr>
              |   "(" <expr> ")"
              |   <func_call>
<binary_expr> ::= <expr> <operator> <expr>
<unary_expr>  ::= <negation> | <unary_op> <expr> | <expr> <unary_op>
<negation>    ::= "-" <expr>
<operator>    ::= "+" | "-" | "*" | "/" | "==" | "!=" | "<" | ">" | "<=" | ">="
<literal>     ::= <int> | <float> | <string> | <bool>
<type>        ::= "int" | "float" | "str" | "bool"
<identifier>  ::= <letter> { <letter> | <digit> | "_" } | "_" { <letter> | <digit> | "_" }
<integer>     ::= <digit>*
<float>       ::= <integer> "." <integer>
<string>      ::= "\"" <character>* "\""
<bool>        ::= "true" | "false"
<func_call>   ::= <identifier> "(" <expr_list> ")"
<expr_list>   ::= <expr> | <expr> "," <expr_list>
<letter>      ::= "a" | "b" | ... | "z" | "A" | "B" | ... | "Z"
<digit>       ::= "0" | "1" | ... | "9"
<character>   ::= any printable ASCII character except "\""