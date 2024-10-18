#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Module {
  // (public, item)
  pub items: Vec<(bool, Item)>,
}

// TODO: generics and traits (trait bounds, default implementations, fields?)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Item {
  Function(Function),
  Struct(Struct),
  Enum(Enum),
  Trait(Trait),
  Operator(Operator),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionHeader {
  pub name: String,
  pub parameters: Vec<Parameter>,
  pub return_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumHeader {
  pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
  pub header: FunctionHeader,
  pub body: Expression,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Operator {
  pub header: OperatorHeader,
  pub body: Expression,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OperatorHeader {
  Prefix {
    operator: String,
    operand: Type,
    result: Type,
  },
  Infix {
    operator: String,
    operands: (Type, Type),
    result: Type,
  },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
  pub header: StructHeader,
  // (public, static, item)
  pub items: Vec<(bool, bool, Item)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructHeader {
  pub name: String,
  pub traits: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Enum {
  pub header: EnumHeader,
  pub variants: Vec<Variant>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Trait {
  pub header: TraitHeader,
  // (public, static, header)
  pub items: Vec<(bool, bool, Header)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitHeader {
  pub name: String,
  pub traits: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Header {
  Function(FunctionHeader),
  Struct(StructHeader),
  Enum(EnumHeader),
  Trait(TraitHeader),
  Operator(OperatorHeader),
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
