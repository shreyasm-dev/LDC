use super::util;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Enum<T> {
  pub header: Header,
  pub variants: Vec<Variant<T>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Header {
  pub name: String,
  pub type_parameters: Vec<util::TypeParameter>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variant<T> {
  pub name: String,
  pub fields: Vec<T>,
}
