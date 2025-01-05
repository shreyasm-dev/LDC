use super::util;

#[derive(Debug, Clone, PartialEq)]
pub struct Function<T> {
  pub header: Header<T>,
  pub body: util::Expression<T>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Header<T> {
  pub name: String,
  pub type_parameters: Vec<util::TypeParameter>,
  pub parameters: Vec<util::Parameter<T>>,
  pub ty: Option<T>,
}
