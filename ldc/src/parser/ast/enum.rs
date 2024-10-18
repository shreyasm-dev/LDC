use super::util;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Enum {
  pub header: Header,
  pub variants: Vec<Variant>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Header {
  pub name: String,
  pub type_parameters: Vec<util::TypeParameter>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variant {
  pub name: String,
  pub fields: Vec<util::Type>,
}
