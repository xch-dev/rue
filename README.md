# Rue Lang

Rue is a typed programming language which gets compiled to [CLVM](https://chialisp.com/clvm) bytecode. It's designed to be an alternative to [Chialisp](https://chialisp.com) for writing on-chain code for the [Chia blockchain](https://chia.net).

## Compilation

There are a series of compiler passes used to construct the final CLVM output:

### Source

Currently, a single file is used as the source for a Rue program. It's read into memory as a UTF-8 encoded string.

### Lexer

The source text is then split into tokens by the lexer. Each token represents things such as punctuation, strings, identifiers, and keywords. This is done to improve performance and enhance error messages during the parser phase.

### Parser

The hand written recursive descent parser is responsible for implementing the language's grammar. You begin top down with the whole program, then for example parse a series of functions, where each function has a parameter list, and so on. Ultimately you end up with a Concrete Syntax Tree (CST) containing your entire program broken up into meaningful segments.

### AST

The CST is not ideal for processing by the compiler since it's untyped and contains tokens it doesn't care about, such as whitespace and keywords. So in this phase, the CST is transformed into an Abstract Syntax Tree (AST), which is a strongly typed representation of all of the parts of the syntax we care about in the compiler and adjacent tooling. However, you can still at any time take an AST node and get its underlying CST node for things such as error reporting.
