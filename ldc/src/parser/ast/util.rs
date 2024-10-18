use crate::{error::ParserError, lexer::token::NumericType};

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
pub struct Parameter {
  pub name: String,
  pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
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
  Named(String),
  Function(Vec<Type>, Box<Type>),
  Tuple(Vec<Type>),
  Array(Box<Type>),
  Union(Box<Type>, Box<Type>),
}

// grouping is not required since (T) == T
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
  Block {
    expressions: Vec<Expression>,
    has_value: bool,
  },
  If {
    condition: Box<Expression>,
    consequence: Box<Expression>,
    alternative: Option<Box<Expression>>,
  },
  While {
    condition: Box<Expression>,
    body: Box<Expression>,
  },
  Return(Box<Expression>),
  Declaration(String),
  // TODO: allow overloading for calling and indexing
  Call {
    expression: Box<Expression>,
    arguments: Vec<Expression>,
  },
  Index {
    expression: Box<Expression>,
    index: Box<Expression>,
  },
  Literal(Literal),
  Infix {
    operator: String,
    operands: (Box<Expression>, Box<Expression>),
  },
  Prefix {
    operator: String,
    operand: Box<Expression>,
  },
  Identifier(String),
}

// TODO: int, float
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
  Char(char),
  String(String),
  Tuple(Vec<Expression>),
  Number(NumberLiteral),
  Array(Vec<Expression>),
  Closure {
    parameters: Vec<Parameter>,
    ty: Option<Type>,
    body: Box<Expression>,
  },
}

#[derive(Debug, Clone, PartialEq)]
pub enum NumberLiteral {
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
}

impl NumberLiteral {
  pub fn from_string(s: &str, ty: NumericType) -> Result<Literal, ParserError> {
    Ok(match ty.clone() {
      NumericType::Char => {
        let n = s
          .parse::<u32>()
          .map_err(|_| ParserError::InvalidNumber(s.to_string(), ty.clone()))?;

        std::char::from_u32(n)
          .map(Literal::Char)
          .ok_or(ParserError::InvalidNumber(s.to_string(), ty))?
      }
      other => Literal::Number(match other {
        NumericType::I8 => s
          .parse()
          .map(NumberLiteral::I8)
          .map_err(|_| ParserError::InvalidNumber(s.to_string(), ty)),
        NumericType::I16 => s
          .parse()
          .map(NumberLiteral::I16)
          .map_err(|_| ParserError::InvalidNumber(s.to_string(), ty)),
        NumericType::I32 => s
          .parse()
          .map(NumberLiteral::I32)
          .map_err(|_| ParserError::InvalidNumber(s.to_string(), ty)),
        NumericType::I64 => s
          .parse()
          .map(NumberLiteral::I64)
          .map_err(|_| ParserError::InvalidNumber(s.to_string(), ty)),
        NumericType::I128 => s
          .parse()
          .map(NumberLiteral::I128)
          .map_err(|_| ParserError::InvalidNumber(s.to_string(), ty)),
        NumericType::U8 => s
          .parse()
          .map(NumberLiteral::U8)
          .map_err(|_| ParserError::InvalidNumber(s.to_string(), ty)),
        NumericType::U16 => s
          .parse()
          .map(NumberLiteral::U16)
          .map_err(|_| ParserError::InvalidNumber(s.to_string(), ty)),
        NumericType::U32 => s
          .parse()
          .map(NumberLiteral::U32)
          .map_err(|_| ParserError::InvalidNumber(s.to_string(), ty)),
        NumericType::U64 => s
          .parse()
          .map(NumberLiteral::U64)
          .map_err(|_| ParserError::InvalidNumber(s.to_string(), ty)),
        NumericType::U128 => s
          .parse()
          .map(NumberLiteral::U128)
          .map_err(|_| ParserError::InvalidNumber(s.to_string(), ty)),
        NumericType::F32 => s
          .parse()
          .map(NumberLiteral::F32)
          .map_err(|_| ParserError::InvalidNumber(s.to_string(), ty)),
        NumericType::F64 => s
          .parse()
          .map(NumberLiteral::F64)
          .map_err(|_| ParserError::InvalidNumber(s.to_string(), ty)),
        _ => unreachable!(),
      }?),
    })
  }
}
