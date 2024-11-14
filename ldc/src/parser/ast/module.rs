use super::{function, operator, r#enum, r#struct, r#trait, util};

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
  pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
  pub modifiers: util::Modifiers,
  pub kind: ItemKind,
}

// TODO: traits (trait bounds, default implementations, fields?)
#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
  Function(function::Function),
  Struct(r#struct::Struct),
  Enum(r#enum::Enum),
  Trait(r#trait::Trait),
  Operator(operator::Operator),
}
