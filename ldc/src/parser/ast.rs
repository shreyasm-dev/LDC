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

// grouping is not required since (T) == T
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expression {
  // (expression, has value)
  Block(Vec<Expression>, bool),
  If(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
  While(Box<Expression>, Box<Expression>),
  Return(Box<Expression>),
  Declaration(String),
  Assignment(Box<Expression>, Box<Expression>),
  DeclarationAssignment(String, Box<Expression>),
  Call(Box<Expression>, Vec<Expression>),
  Field(Box<Expression>, FieldAccess),
  Index(Box<Expression>, Box<Expression>),
  Literal(Literal),
  Infix(Box<Expression>, String, Box<Expression>),
  Prefix(String, Box<Expression>),
  Postfix(Box<Expression>, String),
  Identifier(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FieldAccess {
  Identifier(String),
  Integer(usize),
}

// TODO: int, float
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
  Char(char),
  String(String),
  Tuple(Vec<Expression>),
  Array(Vec<Expression>),
}
