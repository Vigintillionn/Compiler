# Compiler
This is a work in progress and currently only has a primitive lexer and parser.


## Grammer
Below is the grammer for the language written in **BNF (Backus-Naur Form)**.
```math
\begin{align}
&\langle program \rangle ::= &&\langle statement \rangle* \\ \\
&\langle statement \rangle ::= &&\langle declaration \rangle \\
&&& | \langle assignment \rangle
\end{align}
```