pub type Token = ((usize, usize), TokenKind);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
  Whitespace(String),
  Eof,

  Identifier(String),

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
