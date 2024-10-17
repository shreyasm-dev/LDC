#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Module {
  // (public, item)
  pub items: Vec<(bool, Item)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Item {
  Function(Function),
  Struct(Struct),
  Enum(Enum),
  Trait(Trait),
  Operator(Operator),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Header {
  pub name: String,
  pub parameters: Vec<Parameter>,
  pub return_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
  pub header: Header,
  pub body: Expression,
}

// TODO: precedence and associativity
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operator {
  Prefix {
    operator: String,
    operand: Parameter,
    result: Type,
    body: Expression,
  },
  Infix {
    operator: String,
    operands: (Parameter, Parameter),
    result: Type,
    body: Expression,
  },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
  pub name: String,
  // (public, field)
  pub fields: Vec<(bool, Field)>,
  // (public, method)
  pub methods: Vec<(bool, Function)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Enum {
  pub name: String,
  pub variants: Vec<Variant>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Trait {
  pub name: String,
  pub methods: Vec<Header>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Parameter {
  pub name: String,
  pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field {
  pub name: String,
  pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variant {
  pub name: String,
  pub fields: Vec<Type>,
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
  // (expression, has value)
  Block(Vec<Expression>, bool),
  If(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
  While(Box<Expression>, Box<Expression>),
  Return(Box<Expression>),
  Declaration(String),
  // TODO: allow overloading for calling and indexing
  Call(Box<Expression>, Vec<Expression>),
  Index(Box<Expression>, Box<Expression>),
  Literal(Literal),
  Infix(Box<Expression>, String, Box<Expression>),
  Prefix(String, Box<Expression>),
  Identifier(String),
}

// TODO: int, float
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
  Char(char),
  String(String),
  Tuple(Vec<Expression>),
  Array(Vec<Expression>),
  Closure(Vec<Parameter>, Option<Type>, Box<Expression>),
}
