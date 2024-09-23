---
title: Design
layout: home
permalink: /design/
nav_order: 2
---

# Design

- [Design](#design)
  - [Points to Consider](#points-to-consider)
  - [Language Design](#language-design)
    - [Types](#types)
    - [Structures](#structures)
  - [Things to Consider Further](#things-to-consider-further)

## Points to Consider

- Typing
  - Static vs. dynamic typing
  - Strictness (e.g. Rust vs. TypeScript)
  - Inference
  - Types available beyond the primitives
  - Type sizing, signing, and segmentation (e.g. `i32` vs. `u32` vs. `f64` in Rust)
- Structures
  - Classes/structs, enums, traits/interfaces, generics, etc.
- Paradigm
  - Functional, imperative, object-oriented, etc.
- Standard library scope
  - All-encompassing (Java), or minimal (C)
- Syntax
  - Special syntax for standard library structures (e.g. Java vs. Python)
  - Syntactic sugar (e.g. `if let` and blocks with only one expression in Rust)
- Control flow
  - Loops, conditionals, match/switch statements, etc.
- Operators
  - Overloading
- Error handling
  - Monadic (Rust), exception-based (Java, Python etc.), etc.
- Null value
  - No distinction (Java), checked (Swift - `T?` may be of type `T` or `nil`), none, etc.

## Language Design

This section is work-in-progress;

- Source language: Rust
- Typed: Yes (strict, inferred?)
  - Complex compile-time typing for consts? (e.g. even/odd integers)
  - Tuples, functional types, type unions?
  - Type sizing, signing, and segmentation?
- Structures
  - Structs
  - Enums (associated values?)
  - Traits
  - Generics
- Paradigm
  - Functional
- Overloaded operators?
- Error handling
  - Checked (e.g. `T!U`) or monadic
- Null value
  - Checked or monadic

### Types

- `i8`, `u8`, `i16`, `u16`, `i32`, `u32`, `i64`, `u64`, `i128`, `u128`, `f16`, `f32`, `f64`, `f128`
- `boolean` (or maybe as an enum)
- `string` (or maybe in the stdlib)
- `(T, U, V)` - note that the empty tuple, `()`, is the unit type
- `(T, U, V): W` (or `|T, U, V|`)
- `T | U | V`
- `[T]`

### Structures

```rust
struct X: Y {
  pub field: T;

  // this is an instance method called via x.x() or X::x(x)
  fn get(self) -> T {
    self.field // or return x; - everything ultimately evaluates to an expression
  }

  // this is a static method called via X::y()
  fn y() {}
}
```

```rust
enum X {
  X, // X::X
  Y(string), // X::Y("hello")
  Z(boolean, boolean), // X::Z(true, false)
}
```

Traits do not support default or private members.

```rust
trait X {
  pub x: T;

  pub fn x();
}

struct Y: X {
  pub x: T = ...;

  pub fn x() {
    // this is an implementation of the method from the trait X
  }
}
```

```rust
struct X<T> {
  pub x: T;
}

struct Y<T: X> {
  pub x: T;
}
```

```rust
fn +(x: T, y: T) -> T {
  x.add(y)
}
```

## Things to Consider Further

- Do we want to rely more on language features? E.g. using enums for error handling, optionals, and booleans
