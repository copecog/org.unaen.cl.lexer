# org.unaen.cl.lexer
### _Christopher H Cope <christopher.h.cope@gmail.com>_

My first "real" project in Common Lisp -- A somewhat literal implementation according to the mathematical definition of finite automata used by lexers.

The plan is to make something that takes a sort of expression mapping
directly to piece-wise construction of regular expression NFAs (since
regular expression grammar itself is context-free), and then convert
the NFA to a DFA, and the DFA to a generated function that can be used
as a scanner.

After I complete this, I want to write a parser for context free grammars that couples with this scanner so that I can generate scanner/lexer functions for regular expressions and perl-style regular expressions.

See LICENSE.md for licensing details.
