use ariadne::Report;
use std::ops::Range;

pub trait Reportable {
  fn report(&self, span: Range<usize>, name: &'static str) -> Report<'_, (&str, Range<usize>)>;
}
