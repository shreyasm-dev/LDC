use super::{function, operator, r#enum, r#struct, r#trait, util};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Module {
  pub items: Vec<(util::Modifiers, Item)>,
}

// TODO: generics and traits (trait bounds, default implementations, fields?)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Item {
  Function(function::Function),
  Struct(r#struct::Struct),
  Enum(r#enum::Enum),
  Trait(r#trait::Trait),
  Operator(operator::Operator),
}
