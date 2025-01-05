use super::util;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Trait<T> {
  pub header: Header,
  pub items: Vec<(util::Modifiers, Item<T>)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Header {
  pub name: String,
  pub type_parameters: Vec<util::TypeParameter>,
  pub traits: Vec<util::Path>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Item<T> {
  Function(Function<T>),
  Struct(Struct),
  Enum(Enum),
  Trait(Trait_),
  Child(Trait<T>),
  Operator(Operator<T>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function<T> {
  pub name: String,
  pub type_parameters: Vec<util::TypeParameter>,
  pub parameters: Vec<util::Type>,
  pub ty: T,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Enum {
  pub name: String,
  pub type_parameters: Vec<util::TypeParameter>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
  pub name: String,
  pub type_parameters: Vec<util::TypeParameter>,
  pub traits: Vec<util::Path>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Trait_ {
  pub name: String,
  pub type_parameters: Vec<util::TypeParameter>,
  pub traits: Vec<util::Path>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operator<T> {
  Prefix {
    operator: String,
    type_parameters: Vec<util::TypeParameter>,
    operand: T,
    result: T,
  },
  Infix {
    operator: String,
    type_parameters: Vec<util::TypeParameter>,
    operands: (T, T),
    result: T,
  },
}
