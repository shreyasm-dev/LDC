use super::util;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
  pub header: Header,
  pub body: util::Expression,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Header {
  pub name: String,
  pub parameters: Vec<util::Parameter>,
  pub ty: Option<util::Type>,
}
