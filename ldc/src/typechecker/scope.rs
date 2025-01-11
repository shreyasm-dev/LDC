use crate::parser::ast;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use uuid::Uuid;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Scope {
  pub parent: Option<Rc<RefCell<Scope>>>,
  pub items: HashMap<String, Item>,
}

impl Scope {
  pub fn new(parent: Option<Rc<RefCell<Scope>>>) -> Scope {
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
      None => match &self.parent {
        Some(parent) => parent.borrow_mut().set(name, item),
        None => self.insert(name, item),
      },
    }
  }

  // TODO: don't clone
  pub fn get(&self, name: &str) -> Option<Item> {
    match self.items.get(name) {
      Some(item) => Some(item.clone()),
      None => match &self.parent {
        Some(parent) => parent.borrow().get(name),
        None => None,
      },
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item(pub Uuid, pub ItemKind);

impl Item {
  pub fn new(kind: ItemKind) -> Item {
    Item(Uuid::new_v4(), kind)
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
  Function(ast::function::Function<ast::util::Type<Vec<String>>>),
  Struct(ast::r#struct::Struct<ast::util::Type<Vec<String>>>),
  Enum(ast::r#enum::Enum<ast::util::Type<Vec<String>>>),
  Trait(ast::r#trait::Trait<ast::util::Type<Vec<String>>>),
  PrefixOperator(ast::operator::Prefix<ast::util::Type<Vec<String>>>),
  InfixOperator(ast::operator::Infix<ast::util::Type<Vec<String>>>),
  Variable(ast::util::Type<Vec<String>>),
}
