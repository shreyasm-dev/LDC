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
  Op,
  Struct,
  Enum,
  Trait,
  Let,
  Pub,
  Self_,
  While,
  If,
  Else,
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
  pub fn precedence(&self) -> u8 {
    // https://dart.dev/language/operators
    // TODO: prefix -
    let operators = &[
      vec!["..", "..=", "..<"],
      vec!["??"],
      vec!["||"],
      vec!["&&"],
      vec!["==", "!="],
      vec![">=", ">", "<=", "<"],
      vec!["->"],
      vec!["|"],
      vec!["^"],
      vec!["&"],
      vec!["<<", ">>", ">>>"],
      vec!["+", "-", "~"],
      vec!["*", "/", "%"],
    ];

    match self {
      TokenKind::Operator(operator) => {
        for (i, group) in operators.iter().enumerate() {
          if group.contains(&operator.as_str()) {
            return i as u8 + 1;
          }
        }

        0
      }
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
      "op" => TokenKind::Op,
      "struct" => TokenKind::Struct,
      "enum" => TokenKind::Enum,
      "trait" => TokenKind::Trait,
      "let" => TokenKind::Let,
      "pub" => TokenKind::Pub,
      "self" => TokenKind::Self_,
      "while" => TokenKind::While,
      "if" => TokenKind::If,
      "else" => TokenKind::Else,
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

impl std::fmt::Display for TokenKind {
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

        TokenKind::Fn => "fn",
        TokenKind::Op => "op",
        TokenKind::Struct => "struct",
        TokenKind::Enum => "enum",
        TokenKind::Trait => "trait",
        TokenKind::Let => "let",
        TokenKind::Pub => "pub",
        TokenKind::Self_ => "self",
        TokenKind::While => "while",
        TokenKind::If => "if",
        TokenKind::Else => "else",
        TokenKind::Return => "return",

        TokenKind::Char => "char",
        TokenKind::I8 => "i8",
        TokenKind::I16 => "i16",
        TokenKind::I32 => "i32",
        TokenKind::I64 => "i64",
        TokenKind::I128 => "i128",
        TokenKind::U8 => "u8",
        TokenKind::U16 => "u16",
        TokenKind::U32 => "u32",
        TokenKind::U64 => "u64",
        TokenKind::U128 => "u128",
        TokenKind::F16 => "f16",
        TokenKind::F32 => "f32",
        TokenKind::F64 => "f64",
        TokenKind::F128 => "f128",

        TokenKind::LeftParen => "left parenthesis",
        TokenKind::RightParen => "right parenthesis",
        TokenKind::LeftBrace => "left brace",
        TokenKind::RightBrace => "right brace",
        TokenKind::LeftBracket => "left bracket",
        TokenKind::RightBracket => "right bracket",

        TokenKind::Comma => "comma",
        TokenKind::Semicolon => "semicolon",

        TokenKind::Operator(_) => "operator",
      }
    )
  }
}
