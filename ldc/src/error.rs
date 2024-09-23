use crate::util::Reportable;
use ariadne::{ColorGenerator, Fmt, Label, Report, ReportKind, Source};
use std::ops::Range;

#[derive(Debug)]
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

#[derive(Debug)]
pub enum LexerError {
  UnexpectedCharacter(char),
}

impl Reportable for LexerError {
  fn report(
    &self,
    span: Range<usize>,
    name: &'static str,
  ) -> ariadne::Report<'_, (&str, std::ops::Range<usize>)> {
    let mut colors = ColorGenerator::new();
    let a = colors.next();
    let b = colors.next();

    Report::build(ReportKind::Error, &*name, span.start)
      .with_label(
        Label::new((&*name, span))
          .with_message(match self {
            LexerError::UnexpectedCharacter(c) => {
              format!("Unexpected character '{}'", c.fg(a))
            }
          })
          .with_color(b),
      )
      .finish()
  }
}
