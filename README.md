# Compiler
This is a work in progress and currently only has a primitive lexer and parser.


## Grammer
Below is the grammer for the language written in **BNF (Backus-Naur Form)**.
```math
\begin{align}
&\langle program \rangle && \Coloneqq &&\langle statement \rangle* \\
\\
&\langle statement \rangle && \Coloneqq &&\langle declaration \rangle \\
&&&&& | \langle assignment \rangle\\
&&&&& | \langle block \rangle\\
&&&&& | \langle expression \rangle\\
\\
&\langle declaration \rangle && \Coloneqq &&"let" \quad \langle identifier \rangle \quad ":" \quad \langle type \rangle \quad "=" \quad \langle expression \rangle \quad ";" \\
\\
&\langle assignment \rangle && \Coloneqq && \langle identifier \rangle \quad "=" \quad \langle expression \rangle \\
\\
&\langle block \rangle && \Coloneqq && "\{" \quad \langle statement \rangle * \quad "\}" \\
\\
&\langle expression \rangle && \Coloneqq && \langle literal \rangle \\
&&&&& | \langle identifier \rangle \\
&&&&& | \langle expression \rangle \quad "+" \quad \langle expression \rangle \\
&&&&& | \langle expression \rangle \quad "-" \quad \langle expression \rangle \\
&&&&& | \langle expression \rangle \quad "*" \quad \langle expression \rangle \\
&&&&& | \langle expression \rangle \quad "/" \quad \langle expression \rangle \\
\\
&\langle parameter-list \rangle && \Coloneqq && \langle parameter \rangle \quad ("," \quad \langle parameter \rangle)*\\
\\
&\langle parameter \rangle && \Coloneqq && \langle identifier \rangle \quad ":" \quad \langle type \rangle \\
\\
&\langle type \rangle && \Coloneqq && "int" | "uint" | "bool" | "string" | "void" \\
&&&&& | "fn" \quad "(" \quad \langle parameter-list \rangle \quad ")" \quad "\to" \quad \langle type \rangle

\end{align}
```