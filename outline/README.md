---
title: Outline
layout: home
permalink: /outline/
nav_order: 1
---

# Outline

- [Outline](#outline)
  - [Recap](#recap)
  - [What's Being Done Differently This Year?](#whats-being-done-differently-this-year)
  - [The Plan](#the-plan)

## Recap

Last year, we touched upon:

- Types and elements of a language
- Lexers, parsers, context-free grammars
- Compilers and interpreters (including JIT)
- Type systems
- Memory management
- Calling conventions
- Tooling (ANTLR et al.)
- Lambda calculus/functional programming

## What's Being Done Differently This Year?

- Learn by doing: more interactive meetings
- Better organisation and planning via hindsight
- More input

## The Plan

We'll be primarily looking at general-purpose languages, although we may also touch upon DSLs. Unless time permits and there is interest, we probably won't be creating something very low-level.

1. Begin by designing a language (and standard library)
2. Lexer and parser
3. Type system?
4. Interpreter
5. LLVM backend
6. JVM backend?
7. JIT compiler?
8. Native interop (à la JNI)?
9. Bootstrapping?

Some of these are either ambitious or effectively mutually exclusive, and we may not get to implementing them at all; however, we'll still try to look at how they're done in other languages.

A notable absence from this list is memory management. I don't have much experience with implementing memory management in programming languages, and our ability to implement it is affected by the source language we choose. We'll cross that bridge if/when we get to it.

Minor addenda that we may cover briefly if at all:

- Macros and decorators
- Name mangling
- Package management
- Parser/lexer tooling (e.g. ANTLR)
- Development tooling (e.g. with tree-sitter)
- Reflection
