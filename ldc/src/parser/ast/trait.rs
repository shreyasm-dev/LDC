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
  pub parameters: Vec<util::Type>,
  pub ty: util::Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Enum {
  pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
  pub name: String,
  pub traits: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Trait_ {
  pub name: String,
  pub traits: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operator {
  Prefix {
    operator: String,
    operand: util::Type,
    result: util::Type,
  },
  Infix {
    operator: String,
    operands: (util::Type, util::Type),
    result: util::Type,
  },
}
