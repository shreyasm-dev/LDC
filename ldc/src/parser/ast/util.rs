#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Modifiers {
  pub public: bool,
  pub static_: bool,
}

impl Default for Modifiers {
  fn default() -> Self {
    Modifiers {
      public: false,
      static_: false,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Parameter {
  pub name: String,
  pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
  I8,
  I16,
  I32,
  I64,
  I128,
  U8,
  U16,
  U32,
  U64,
  U128,
  F16,
  F32,
  F64,
  F128,
  Char,
  Named(String),
  Function(Vec<Type>, Box<Type>),
  Tuple(Vec<Type>),
  Array(Box<Type>),
  Union(Box<Type>, Box<Type>),
}

// grouping is not required since (T) == T
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expression {
  Block {
    expressions: Vec<Expression>,
    has_value: bool,
  },
  If {
    condition: Box<Expression>,
    consequence: Box<Expression>,
    alternative: Option<Box<Expression>>,
  },
  While {
    condition: Box<Expression>,
    body: Box<Expression>,
  },
  Return(Box<Expression>),
  Declaration(String),
  // TODO: allow overloading for calling and indexing
  Call {
    expression: Box<Expression>,
    arguments: Vec<Expression>,
  },
  Index {
    expression: Box<Expression>,
    index: Box<Expression>,
  },
  Literal(Literal),
  Infix {
    operator: String,
    operands: (Box<Expression>, Box<Expression>),
  },
  Prefix {
    operator: String,
    operand: Box<Expression>,
  },
  Identifier(String),
}

// TODO: int, float
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
  Char(char),
  String(String),
  Tuple(Vec<Expression>),
  Array(Vec<Expression>),
  Closure {
    parameters: Vec<Parameter>,
    ty: Option<Type>,
    body: Box<Expression>,
  },
}
