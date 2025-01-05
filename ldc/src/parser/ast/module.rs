use super::{function, operator, r#enum, r#struct, r#trait, util};

#[derive(Debug, Clone, PartialEq)]
pub struct Module<T> {
  pub items: Vec<Item<T>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item<T> {
  pub modifiers: util::Modifiers,
  pub kind: ItemKind<T>,
}

// TODO: traits (trait bounds, default implementations, fields?)
#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind<T> {
  Function(function::Function<T>),
  Struct(r#struct::Struct<T>),
  Enum(r#enum::Enum<T>),
  Trait(r#trait::Trait<T>),
  Operator(operator::Operator<T>),
}
