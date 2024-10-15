use std::ops::Range;

pub type Token = (Range<usize>, TokenKind);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
  Whitespace(String),
  Comment(String),
  Eof,

  Identifier(String),
  StringLiteral(String),

  Fn,
  Struct,
  Enum,
  Trait,
  Let,
  Pub,
  Self_,
  While,
  If,
  Return,

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
  F16,
  F32,
  F64,
  F128,

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

impl TokenKind {
  pub fn from_identifier(ident: String) -> Self {
    match ident.as_str() {
      "fn" => TokenKind::Fn,
      "struct" => TokenKind::Struct,
      "enum" => TokenKind::Enum,
      "trait" => TokenKind::Trait,
      "let" => TokenKind::Let,
      "pub" => TokenKind::Pub,
      "self" => TokenKind::Self_,
      "while" => TokenKind::While,
      "if" => TokenKind::If,
      "return" => TokenKind::Return,

      "char" => TokenKind::Char,
      "i8" => TokenKind::I8,
      "i16" => TokenKind::I16,
      "i32" => TokenKind::I32,
      "i64" => TokenKind::I64,
      "i128" => TokenKind::I128,
      "u8" => TokenKind::U8,
      "u16" => TokenKind::U16,
      "u32" => TokenKind::U32,
      "u64" => TokenKind::U64,
      "u128" => TokenKind::U128,
      "f16" => TokenKind::F16,
      "f32" => TokenKind::F32,
      "f64" => TokenKind::F64,
      "f128" => TokenKind::F128,

      _ => TokenKind::Identifier(ident),
    }
  }
}
