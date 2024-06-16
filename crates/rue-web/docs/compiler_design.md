# Compiler Design

At a high level, the responsibility of the Rue compiler is to translate source text into [CLVM](https://chialisp.com/clvm) bytecode. CLVM is a virtual machine run by the [Chia blockchain](https://chia.net), which can interpret this bytecode and calculate the output of a spent smart coin.

## Compilation Phases

The compiler is split into various compilation passes.

### Lexer

In order for source text to be more easily parsed, it's important to split it into a series of lexical tokens first. This means the parser doesn't need to backtrack and doesn't have to worry about the structure of the text itself, which improves performance.

### Parser

Once the hand-written [recursive descent](https://en.wikipedia.org/wiki/Recursive_descent_parser) parser has a stream of tokens to work with, it can parse them into a [concrete syntax tree](https://en.wikipedia.org/wiki/Parse_tree). This parse tree still contains all information about the source text, including whitespace and comments.

### AST

The parse tree is not very convenient to work with during the compilation process, since it contains unnecessary trivia (whitespace and comments). It's also completely untyped, making it error prone if you work with it directly. For example, if you change the way the parse tree is structured, there could be several places in the code you'd need to update the usages of it.

The [abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) (AST) is a strongly typed view into the parse tree, with different structs representing the various nodes. Each AST node has methods for extracting its relevant components (for example, the identifier and parameters of a function), but the individual tokens like whitespace are typically skipped over and not exposed. That said, you can still retrieve a view into the underlying parse tree if needed.

### HIR

Now that there is a convenient AST to work with, we can implement the compiler on top of it. The first step is lowering the AST into the high-level intermediate representation (HIR). This compiler pass is responsible for the following things:

- Building a hierarchy of nested scopes, which are attached to functions and blocks.
- Declaring symbols and resolving identifiers which point to them.
- Inferring types based on context and values, and type checking expressions and symbols.
- Producing compiler errors for incorrectly structured code, undefined references, and type errors.
- Determining what the meaning of code is based on the type.

### LIR

While the HIR represents our code's intent pretty closely, we now need to translate it into something closer to what CLVM can understand. In addition to [resolving captures](#resolving-captures), code optimizations are applied during this phase.

Here are some example optimizations that can be taken:

- Constant expressions can be evaluated at compile time and simplified.
- Eliminating unused code (warnings are emitted when symbols are unused).
- Unnecessary scopes can be reduced.

### Codegen

Finally, the LIR is translated to the final CLVM output.

## Resolving Captures

CLVM has no concept of scopes, and instead uses an [environment](https://chialisp.com/clvm/#environment) to resolve references. It's a single binary tree CLVM value which you can index into with an atom (known as a path). However, Rue uses a hierarchy to resolve symbols, which can possibly be defined in external scopes.

It's necessary to automatically pass in these values into the child environment and resolve the symbols to an appropriate path. Additionally, if you pass a function which captures external symbols around as a value, it must be wrapped in such a way that its external environment is curried into it. This is called a closure.

### Scope Dependency Graph

Because functions can recursively reference each other, while also being defined out of order, it can be a quite complicated to determine which symbols are captured by which scopes. Simply recursively checking the scopes to find the captures is insufficient, since you can easily overflow the stack or end up with an incomplete environment when you retrace an earlier scope.

As such, it is necessary to determine the relationships between various scopes beforehand in a separate pass, so you can walk through each scope only once and update all of the dependent environments to include the capture.
