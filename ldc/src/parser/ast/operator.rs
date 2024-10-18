use super::util;

#[derive(Debug, Clone, PartialEq)]
pub struct Operator {
  pub header: Header,
  pub body: util::Expression,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Header {
  Prefix {
    operator: String,
    operand: util::Parameter,
    result: util::Type,
  },
  Infix {
    operator: String,
    operands: (util::Parameter, util::Parameter),
    result: util::Type,
  },
}
