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
pub struct Function {
  pub name: String,
  pub parameters: Vec<Parameter>,
  pub return_type: Type,
  pub body: Expression,
}

// TODO: precedence and associativity
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operator {
  Unary {
    operator: String,
    operand: Type,
    result: Type,
    body: Expression,
  },
  Binary {
    operator: String,
    left: Type,
    right: Type,
    result: Type,
    body: Expression,
  },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
  pub name: String,
  // (public, field)
  pub fields: Vec<(bool, Field)>,
  pub methods: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Enum {
  pub name: String,
  pub variants: Vec<Variant>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Trait {
  pub name: String,
  pub methods: Vec<Function>,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expression {
  Block(Vec<Expression>),
  If(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
  While(Box<Expression>, Box<Expression>),
  Return(Box<Expression>),
  Declaration(String),
  Assignment(String, Box<Expression>),
  Call(Box<Expression>, Vec<Expression>),
  Field(Box<Expression>, String), // TODO: '.' is not handled specially
  Index(Box<Expression>, Box<Expression>),
  Literal(Literal),
  Binary(Box<Expression>, String, Box<Expression>),
  Prefix(String, Box<Expression>),
  Postfix(Box<Expression>, String),
  Identifier(String),
}

// TODO: int, float
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
  Char(char),
  String(String),
  Tuple(Vec<Literal>),
  Array(Vec<Literal>),
}
