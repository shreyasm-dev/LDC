use super::util;

#[derive(Debug, Clone, PartialEq)]
pub struct Operator<T> {
  pub header: Header<T>,
  pub body: util::Expression<T>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Header<T> {
  Prefix(Prefix<T>),
  Infix(Infix<T>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Prefix<T> {
  pub operator: String,
  // pub type_parameters: Vec<util::TypeParameter>,
  pub operand: util::Parameter<T>,
  pub result: T,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Infix<T> {
  pub operator: String,
  // pub type_parameters: Vec<util::TypeParameter>,
  pub operands: (util::Parameter<T>, util::Parameter<T>),
  pub result: T,
}
