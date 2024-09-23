use std::fmt;

use crate::util::Span;

#[derive(Debug)]
pub enum LexerError {
  UnexpectedCharacter(Span, char),
}

impl fmt::Display for LexerError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      LexerError::UnexpectedCharacter(span, c) => {
        write!(f, "Unexpected character '{}' at {}", c, span.0)
      }
    }
  }
}
