use std::ops::Range;

pub type Token = (Range<usize>, TokenKind);

#[derive(Debug, Clone, PartialEq, Eq)]
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

impl TokenKind {
  pub fn from_identifier(ident: String) -> Self {
    match ident.as_str() {
      "fn" => TokenKind::Fn,
      "struct" => TokenKind::Struct,
      "enum" => TokenKind::Enum,
      "trait" => TokenKind::Trait,
      "let" => TokenKind::Let,

      "bool" => TokenKind::Bool,

      _ => TokenKind::Identifier(ident),
    }
  }
}
