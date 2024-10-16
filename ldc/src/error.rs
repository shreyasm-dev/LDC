use ariadne::{ColorGenerator, Fmt, Label, Report, ReportKind, Source};
use std::ops::{Range, RangeInclusive};

use crate::lexer::token::TokenKind;

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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserError {
  UnexpectedToken(Option<TokenKind>, Vec<TokenKind>),
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
                .map(|c| format!("{}", c).fg(b).to_string())
                .collect::<Vec<_>>();

              format!(
                "Unexpected {}{}",
                match token {
                  None | Some(TokenKind::Eof) => "end of input".to_string(),
                  Some(t) => format!("{}", t).fg(b).to_string(),
                },
                if items.is_empty() {
                  "".to_string()
                } else {
                  format!(", expected {}", items.join(", "))
                }
              )
            }
          })
          .with_color(b),
      )
      .finish()
  }
}
