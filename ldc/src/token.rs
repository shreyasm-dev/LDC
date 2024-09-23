use std::ops::Range;

pub type Token = (Range<usize>, TokenKind);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
  Whitespace(String),
  Eof,

  Identifier(String),
  StringLiteral(String),

  Fn,
  Struct,
  Enum,
  Trait,
  Let,

  Bool,

  LeftParen,
  RightParen,
  LeftBrace,
  RightBrace,
  LeftBracket,
  RightBracket,
  LeftAngle,
  RightAngle,

  Comma,
  Colon,
  Semicolon,
  Dot,
}
