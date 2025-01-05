use crate::{error::ParserError, lexer::token::NumericType, map0, map1};
use std::{collections::BTreeSet, fmt::Display, marker::PhantomData};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Modifiers {
  pub public: bool,
  pub static_: bool,
}

impl Default for Modifiers {
  fn default() -> Self {
    Modifiers {
      public: false,
      static_: false,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Parameter<T> {
  pub name: String,
  pub ty: T,
}

#[derive(Debug, Clone, Eq, Hash, PartialOrd, Ord)]
pub enum Type {
  Bool,
  I8,
  I16,
  I32,
  I64,
  I128,
  U8,
  U16,
  U32,
  U64,
  U128,
  F32,
  F64,
  Char,
  Named(String, Vec<Type>),
  Function(Vec<Type>, Box<Type>),
  Tuple(Vec<Type>),
  Array(Box<Type>),
  Union(BTreeSet<Type>), // the parser guarantees that the union is flat (no unions of unions) and that there is at least one type
}

impl Type {
  pub fn reduce(&self) -> Type {
    match self {
      Type::Named(name, parameters) => {
        Type::Named(name.clone(), parameters.iter().map(Type::reduce).collect())
      }
      Type::Function(parameters, ty) => Type::Function(
        parameters.iter().map(Type::reduce).collect(),
        Box::new(ty.reduce()),
      ),
      Type::Tuple(types) => {
        if types.len() == 1 {
          types[0].clone()
        } else {
          Type::Tuple(types.iter().map(Type::reduce).collect())
        }
      }
      Type::Array(ty) => Type::Array(Box::new(ty.reduce())),
      Type::Union(types) => {
        Type::Union(types.iter().map(|ty| ty.reduce()).collect::<BTreeSet<_>>())
      }
      _ => self.clone(),
    }
  }

  pub fn satisfies(&self, other: &Type) -> bool {
    println!(
      "{} ({}) satisfies {} ({})",
      self.reduce(),
      self,
      other.reduce(),
      other
    );
    match (self.reduce(), other.reduce()) {
      (Type::Named(a, _), Type::Named(b, _)) => a == b, // TODO: traits, parameters
      (Type::Function(a, b), Type::Function(c, d)) => {
        a.len() == c.len() && a.iter().zip(c.iter()).all(|(a, c)| a.satisfies(c)) && b.satisfies(&d)
      }
      (Type::Tuple(a), Type::Tuple(b)) => {
        a.len() == b.len() && a.iter().zip(b.iter()).all(|(a, b)| a.satisfies(b))
      }
      (Type::Array(a), Type::Array(b)) => a.satisfies(&b),

      // if a union `a` satisfies a type `b`, then all types in `a` must satisfy `b`
      (Type::Union(a), b) => a.iter().all(|ty| ty.satisfies(&b)),
      // if a type `a` satisfies a union `b`, then `a` must satisfy at least one type in `b`
      (a, Type::Union(b)) => b.iter().any(|ty| a.satisfies(ty)),

      _ => self == other,
    }
  }
}

impl PartialEq for Type {
  fn eq(&self, other: &Self) -> bool {
    match (self.reduce(), other.reduce()) {
      // general cases
      (Type::Bool, Type::Bool)
      | (Type::I8, Type::I8)
      | (Type::I16, Type::I16)
      | (Type::I32, Type::I32)
      | (Type::I64, Type::I64)
      | (Type::I128, Type::I128)
      | (Type::U8, Type::U8)
      | (Type::U16, Type::U16)
      | (Type::U32, Type::U32)
      | (Type::U64, Type::U64)
      | (Type::U128, Type::U128)
      | (Type::F32, Type::F32)
      | (Type::F64, Type::F64)
      | (Type::Char, Type::Char) => true,
      (Type::Named(a, b), Type::Named(c, d)) => a == c && b == d, // TODO: traits
      (Type::Function(a, b), Type::Function(c, d)) => a == c && b == d,
      (Type::Tuple(a), Type::Tuple(b)) => a == b,
      (Type::Array(a), Type::Array(b)) => a == b,
      (Type::Union(a), Type::Union(b)) => a == b,

      // (t) == t, vice versa
      // (Type::Tuple(a), b) | (b, Type::Tuple(a)) if a.len() == 1 => a[0] == b,
      // not necessary since we reduce the types before comparing
      _ => false,
    }
  }
}

impl Display for Type {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Type::Bool => write!(f, "bool"),
      Type::I8 => write!(f, "i8"),
      Type::I16 => write!(f, "i16"),
      Type::I32 => write!(f, "i32"),
      Type::I64 => write!(f, "i64"),
      Type::I128 => write!(f, "i128"),
      Type::U8 => write!(f, "u8"),
      Type::U16 => write!(f, "u16"),
      Type::U32 => write!(f, "u32"),
      Type::U64 => write!(f, "u64"),
      Type::U128 => write!(f, "u128"),
      Type::F32 => write!(f, "f32"),
      Type::F64 => write!(f, "f64"),
      Type::Char => write!(f, "char"),
      Type::Named(name, parameters) => {
        write!(f, "{}", name)?;
        if !parameters.is_empty() {
          write!(f, "<")?;
          for (i, parameter) in parameters.iter().enumerate() {
            if i != 0 {
              write!(f, ", ")?;
            }
            write!(f, "{}", parameter)?;
          }
          write!(f, ">")?;
        }
        Ok(())
      }
      Type::Function(parameters, return_type) => {
        write!(f, "(")?;
        for (i, parameter) in parameters.iter().enumerate() {
          if i != 0 {
            write!(f, ", ")?;
          }
          write!(f, "{}", parameter)?;
        }
        write!(f, "): {}", return_type)
      }
      Type::Tuple(types) => {
        write!(f, "(")?;
        for (i, ty) in types.iter().enumerate() {
          if i != 0 {
            write!(f, ", ")?;
          }
          write!(f, "{}", ty)?;
        }
        write!(f, ")")
      }
      Type::Array(ty) => write!(f, "[{}]", ty),
      Type::Union(types) => {
        for (i, ty) in types.iter().enumerate() {
          if i != 0 {
            write!(f, " | ")?;
          }

          write!(f, "{}", ty)?;
        }

        Ok(())
      }
    }
  }
}

// grouping is not required since (T) == T
#[derive(Debug, Clone, PartialEq)]
pub enum Expression<T> {
  Block {
    expressions: Vec<Expression<T>>,
    has_value: bool,
  },
  If {
    condition: Box<Expression<T>>,
    consequence: Box<Expression<T>>,
    alternative: Option<Box<Expression<T>>>,
  },
  While {
    condition: Box<Expression<T>>,
    body: Box<Expression<T>>,
  },
  Return(Box<Expression<T>>),
  Declaration(String),
  // TODO: allow overloading for calling and indexing
  Call {
    expression: Box<Expression<T>>,
    arguments: Vec<Expression<T>>,
  },
  Index {
    expression: Box<Expression<T>>,
    index: Box<Expression<T>>,
  },
  Literal(Literal<T>),
  Infix {
    operator: String,
    operands: (Box<Expression<T>>, Box<Expression<T>>),
  },
  Prefix {
    operator: String,
    operand: Box<Expression<T>>,
  },
  Identifier(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal<T> {
  Char(char),
  String(String),
  Tuple(Vec<Expression<T>>),
  Number(NumberLiteral<T>),
  Array(Vec<Expression<T>>),
  Bool(bool),
  Closure {
    parameters: Vec<Parameter<T>>,
    ty: Option<Type>,
    body: Box<Expression<T>>,
  },
}

#[derive(Debug, Clone, PartialEq)]
pub enum NumberLiteral<T> {
  I8(i8),
  I16(i16),
  I32(i32),
  I64(i64),
  I128(i128),
  U8(u8),
  U16(u16),
  U32(u32),
  U64(u64),
  U128(u128),
  F32(f32),
  F64(f64),
  _PhantomData(PhantomData<T>),
}

impl<T> NumberLiteral<T> {
  pub fn from_string(s: &str, ty: NumericType) -> Result<Literal<T>, ParserError> {
    Ok(match ty.clone() {
      NumericType::Char => {
        let n = s
          .parse::<u32>()
          .map_err(|_| ParserError::InvalidNumber(s.to_string(), ty.clone()))?;

        std::char::from_u32(n)
          .map(Literal::Char)
          .ok_or(ParserError::InvalidNumber(s.to_string(), ty))?
      }
      other => Literal::Number(map0!(
        other,
        NumericType,
        NumberLiteral<T>,
        |n| s
          .parse()
          .map(n)
          .map_err(|_| ParserError::InvalidNumber(s.to_string(), ty)),
        I8,
        I16,
        I32,
        I64,
        I128,
        U8,
        U16,
        U32,
        U64,
        U128,
        F32,
        F64
      )?),
    })
  }
}

impl Into<Type> for NumberLiteral<Type> {
  fn into(self) -> Type {
    map1!(
      self,
      NumberLiteral,
      Type,
      |n| n,
      I8,
      I16,
      I32,
      I64,
      I128,
      U8,
      U16,
      U32,
      U64,
      U128,
      F32,
      F64
    )
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeParameter {
  pub name: String,
  pub traits: Vec<Path>,
}

pub type Path = Vec<String>;
