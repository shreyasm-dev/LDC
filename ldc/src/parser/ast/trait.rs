use super::util;

// TODO: local sub-traits
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Trait {
  pub header: Header,
  pub items: Vec<(util::Modifiers, Item)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Header {
  pub name: String,
  pub type_parameters: Vec<util::TypeParameter>,
  pub traits: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Item {
  Function(Function),
  Struct(Struct),
  Enum(Enum),
  Trait(Trait_),
  Operator(Operator),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
  pub name: String,
  pub type_parameters: Vec<util::TypeParameter>,
  pub parameters: Vec<util::Type>,
  pub ty: util::Type,
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
  pub traits: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Trait_ {
  pub name: String,
  pub type_parameters: Vec<util::TypeParameter>,
  pub traits: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operator {
  Prefix {
    operator: String,
    type_parameters: Vec<util::TypeParameter>,
    operand: util::Type,
    result: util::Type,
  },
  Infix {
    operator: String,
    type_parameters: Vec<util::TypeParameter>,
    operands: (util::Type, util::Type),
    result: util::Type,
  },
}
