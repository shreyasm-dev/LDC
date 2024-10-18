use super::util;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
  pub header: Header,
  pub body: util::Expression,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Header {
  pub name: String,
  pub type_parameters: Vec<util::TypeParameter>,
  pub parameters: Vec<util::Parameter>,
  pub ty: Option<util::Type>,
}
