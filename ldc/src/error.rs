use crate::lexer::token::{NumericType, TokenKind};
use ariadne::{ColorGenerator, Fmt, Label, Report, ReportKind, Source};
use std::ops::{Range, RangeInclusive};

pub trait Reportable {
  fn report(&self, span: Range<usize>, name: &'static str) -> Report<'_, (&str, Range<usize>)>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error<T: Reportable>(pub Range<usize>, pub T);

impl<T: Reportable> Error<T> {
  pub fn print(&self, source: &str, name: &'static str) {
    self
      .1
      .report(self.0.clone(), name)
      .print((name, Source::from(source)))
      .unwrap();
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexerError {
  UnexpectedCharacter(char, &'static [RangeInclusive<char>], &'static [char]),
  UnexpectedEof(usize),
  InvalidCodepoint(String),
  InvalidNumericType(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserError {
  UnexpectedToken(Option<TokenKind>, Vec<TokenKind>),
  InvalidNumber(String, NumericType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypecheckerError<T> {
  InvalidType { expected: T, found: T },
  InvalidArguments { expected: Vec<T>, found: Vec<T> },
  UnresolvedIdentifier(String),
}

impl Reportable for LexerError {
  fn report(
    &self,
    span: Range<usize>,
    name: &'static str,
  ) -> ariadne::Report<'_, (&str, std::ops::Range<usize>)> {
    let mut colors = ColorGenerator::new();
    colors.next();
    let b = colors.next();

    let builder = Report::build(ReportKind::Error, &*name, span.start).with_label(
      Label::new((&*name, span.clone()))
        .with_message(match self {
          LexerError::UnexpectedCharacter(c, ranges, expected) => {
            let items = vec![
              expected
                .iter()
                .map(|c| c.fg(b).to_string())
                .collect::<Vec<_>>(),
              ranges
                .iter()
                .map(|r| format!("{}-{}", r.start().fg(b), r.end().fg(b)))
                .collect::<Vec<_>>(),
            ]
            .concat();

            format!(
              "Unexpected character {}{}",
              c.fg(b),
              if items.is_empty() {
                "".to_string()
              } else {
                format!(", expected {}", items.join(", "))
              }
            )
          }
          LexerError::UnexpectedEof(_) => "Unexpected end of input".to_string(),
          LexerError::InvalidCodepoint(codepoint) => {
            format!("Invalid code point 0x{}", codepoint)
          }
          LexerError::InvalidNumericType(ty) => format!("Invalid numeric type {}", ty),
        })
        .with_color(b),
    );

    match self {
      LexerError::UnexpectedEof(start) => builder
        .with_label(
          Label::new((&*name, *start..*start))
            .with_message("Token began here")
            .with_color(b),
        )
        .finish(),
      _ => builder.finish(),
    }
  }
}

impl Reportable for ParserError {
  fn report(
    &self,
    span: Range<usize>,
    name: &'static str,
  ) -> ariadne::Report<'_, (&str, std::ops::Range<usize>)> {
    let mut colors = ColorGenerator::new();
    colors.next();
    let b = colors.next();

    Report::build(ReportKind::Error, &*name, span.start)
      .with_label(
        Label::new((&*name, span.clone()))
          .with_message(match self {
            ParserError::UnexpectedToken(token, expected) => {
              let items = expected
                .iter()
                .map(|c| c.fg(b).to_string())
                .collect::<Vec<_>>();

              format!(
                "Unexpected {}{}",
                match token {
                  None | Some(TokenKind::Eof) => "end of input".to_string(),
                  Some(t) => t.fg(b).to_string(),
                },
                if items.is_empty() {
                  "".to_string()
                } else {
                  format!(", expected {}", items.join(", "))
                }
              )
            }
            ParserError::InvalidNumber(s, ty) => format!("Invalid number {} for type {}", s, ty),
          })
          .with_color(b),
      )
      .finish()
  }
}
