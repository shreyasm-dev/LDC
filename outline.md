# Outline

- [Outline](#outline)
  - [Recap](#recap)
  - [What's Being Done Differently This Year?](#whats-being-done-differently-this-year)
  - [The Plan](#the-plan)

## Recap

Last year, we touched upon:

- Types and elements of a language
- Lexers, parsers, context-free grammars
- Type systems
- Memory management
- Calling conventions
- Tooling (ANTLR et al.)
- Lamda calculus/functional programming

## What's Being Done Differently This Year?

- Learn by doing: more interactive meetings
- Better organisation and planning via hindsight
- More input

## The Plan

1. Begin by designing a language (and stdlib)
2. Lexer and parser
3. Type system?
4. Interpreter
5. LLVM backend
6. JVM backend?
7. JIT compiler?
8. Native interop (à la JNI)?
9. Macros?
10. Bootstraping?

Some of these are either ambitious or effectively mutually exclusive, and we may not get to implementing them at all; however, we'll still try to look at how they're done in other languages.

A notable absence from this list is memory management. I don't have much experience implementing GCs et al., and our ability to implement it is affected by the source language we choose. We'll cross that bridge if/when we get to it.

Minor addenda that we may cover briefly if at all:

- Name mangling
- Package management
- Development tooling (e.g. via tree-sitter)
- Reflection
