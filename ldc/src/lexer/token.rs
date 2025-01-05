use std::ops::Range;

pub type Token = (Range<usize>, TokenKind);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
  Whitespace(String),
  Comment(String),
  Eof,

  Identifier(String),
  StringLiteral(String),
  CharLiteral(char),
  NumberLiteral(String, NumericType),

  Fn,
  Struct,
  Enum,
  Trait,
  Let,
  Pub,
  Self_,
  Static,
  While,
  If,
  Else,
  Return,
  Bool,
  True,
  False,

  Numeric(NumericType),

  LeftParen,
  RightParen,
  LeftBrace,
  RightBrace,
  LeftBracket,
  RightBracket,

  Comma,
  Semicolon,

  Operator(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NumericType {
  Char,
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
}

impl TokenKind {
  pub fn prefix_precedence(&self) -> u8 {
    match self {
      TokenKind::Operator(operator) => match operator.as_str() {
        "+" | "-" | "~" | "!" => 14,
        _ => 0,
      },
      _ => 0,
    }
  }

  pub fn infix_precedence(&self) -> u8 {
    // https://dart.dev/language/operators
    match self {
      TokenKind::Operator(operator) => match operator.as_str() {
        "." | "::" => 15,
        "*" | "/" | "%" => 13,
        "+" | "-" => 12,
        "<<" | ">>" | ">>>" => 11,
        "&" => 10,
        "^" => 9,
        "|" => 8,
        "->" => 7,
        "<" | "<=" | ">" | ">=" => 6,
        "==" | "!=" => 5,
        "&&" => 4,
        "||" => 3,
        "??" => 2,
        ".." | "..=" | "..<" => 1,
        _ => 0,
      },
      _ => 0,
    }
  }

  pub fn left_associative(&self) -> bool {
    match self {
      TokenKind::Operator(operator) => match operator.as_str() {
        "=" => false,
        _ => true,
      },
      _ => false,
    }
  }

  pub fn from_identifier(ident: String) -> Self {
    match ident.as_str() {
      "fn" => TokenKind::Fn,
      "struct" => TokenKind::Struct,
      "enum" => TokenKind::Enum,
      "trait" => TokenKind::Trait,
      "let" => TokenKind::Let,
      "pub" => TokenKind::Pub,
      "self" => TokenKind::Self_,
      "static" => TokenKind::Static,
      "while" => TokenKind::While,
      "if" => TokenKind::If,
      "else" => TokenKind::Else,
      "return" => TokenKind::Return,
      "bool" => TokenKind::Bool,
      "true" => TokenKind::True,
      "false" => TokenKind::False,

      "char" => TokenKind::Numeric(NumericType::Char),
      "i8" => TokenKind::Numeric(NumericType::I8),
      "i16" => TokenKind::Numeric(NumericType::I16),
      "i32" => TokenKind::Numeric(NumericType::I32),
      "i64" => TokenKind::Numeric(NumericType::I64),
      "i128" => TokenKind::Numeric(NumericType::I128),
      "u8" => TokenKind::Numeric(NumericType::U8),
      "u16" => TokenKind::Numeric(NumericType::U16),
      "u32" => TokenKind::Numeric(NumericType::U32),
      "u64" => TokenKind::Numeric(NumericType::U64),
      "u128" => TokenKind::Numeric(NumericType::U128),
      "f32" => TokenKind::Numeric(NumericType::F32),
      "f64" => TokenKind::Numeric(NumericType::F64),

      _ => TokenKind::Identifier(ident),
    }
  }
}

impl std::fmt::Display for TokenKind {
  // TODO
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        TokenKind::Whitespace(_) => "whitespace",
        TokenKind::Comment(_) => "comment",
        TokenKind::Eof => "end of input",

        TokenKind::Identifier(_) => "identifier",
        TokenKind::StringLiteral(_) => "string literal",
        TokenKind::CharLiteral(_) => "character literal",
        TokenKind::NumberLiteral(_, _) => "number literal",

        TokenKind::Fn => "fn",
        TokenKind::Struct => "struct",
        TokenKind::Enum => "enum",
        TokenKind::Trait => "trait",
        TokenKind::Let => "let",
        TokenKind::Pub => "pub",
        TokenKind::Self_ => "self",
        TokenKind::Static => "static",
        TokenKind::While => "while",
        TokenKind::If => "if",
        TokenKind::Else => "else",
        TokenKind::Return => "return",
        TokenKind::Bool => "bool",
        TokenKind::True => "true",
        TokenKind::False => "false",

        TokenKind::Numeric(NumericType::Char) => "char",
        TokenKind::Numeric(NumericType::I8) => "i8",
        TokenKind::Numeric(NumericType::I16) => "i16",
        TokenKind::Numeric(NumericType::I32) => "i32",
        TokenKind::Numeric(NumericType::I64) => "i64",
        TokenKind::Numeric(NumericType::I128) => "i128",
        TokenKind::Numeric(NumericType::U8) => "u8",
        TokenKind::Numeric(NumericType::U16) => "u16",
        TokenKind::Numeric(NumericType::U32) => "u32",
        TokenKind::Numeric(NumericType::U64) => "u64",
        TokenKind::Numeric(NumericType::U128) => "u128",
        TokenKind::Numeric(NumericType::F32) => "f32",
        TokenKind::Numeric(NumericType::F64) => "f64",

        TokenKind::LeftParen => "(",
        TokenKind::RightParen => ")",
        TokenKind::LeftBrace => "{",
        TokenKind::RightBrace => "}",
        TokenKind::LeftBracket => "[",
        TokenKind::RightBracket => "]",

        TokenKind::Comma => ",",
        TokenKind::Semicolon => ";",

        TokenKind::Operator(op) =>
          if op.is_empty() {
            "operator"
          } else {
            op
          },
      }
    )
  }
}

impl std::fmt::Display for NumericType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        NumericType::Char => "char",
        NumericType::I8 => "i8",
        NumericType::I16 => "i16",
        NumericType::I32 => "i32",
        NumericType::I64 => "i64",
        NumericType::I128 => "i128",
        NumericType::U8 => "u8",
        NumericType::U16 => "u16",
        NumericType::U32 => "u32",
        NumericType::U64 => "u64",
        NumericType::U128 => "u128",
        NumericType::F32 => "f32",
        NumericType::F64 => "f64",
      }
    )
  }
}
