use crate::parser::ast;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
  pub parent: Option<Box<Scope>>,
  pub items: HashMap<String, Item>,
}

impl Scope {
  pub fn new(parent: Option<Box<Scope>>) -> Scope {
    Scope {
      parent,
      items: HashMap::new(),
    }
  }

  pub fn insert(&mut self, name: String, item: Item) {
    self.items.insert(name, item);
  }

  pub fn set(&mut self, name: String, item: Item) {
    match self.items.get_mut(&name) {
      Some(i) => *i = item,
      None => match &mut self.parent {
        Some(parent) => parent.set(name, item),
        None => self.insert(name, item),
      },
    }
  }

  pub fn get(&self, name: &str) -> Option<&Item> {
    match self.items.get(name) {
      Some(item) => Some(item),
      None => match &self.parent {
        Some(parent) => parent.get(name),
        None => None,
      },
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
  // TODO: variables
  Function(ast::function::Function<ast::util::Type>),
  Struct(ast::r#struct::Struct<ast::util::Type>),
  Enum(ast::r#enum::Enum<ast::util::Type>),
  Trait(ast::r#trait::Trait<ast::util::Type>),
  PrefixOperator(ast::operator::Prefix<ast::util::Type>),
  InfixOperator(ast::operator::Infix<ast::util::Type>),
  TypeParameter(ast::util::TypeParameter),
  Variable(ast::util::Type),
}
