use super::{module, util};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
  pub header: Header,
  pub items: Vec<(util::Modifiers, module::Item)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Header {
  pub name: String,
  pub traits: Vec<String>,
}
