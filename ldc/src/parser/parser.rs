use super::ast::{
  Enum, Expression, FieldAccess, Function, Item, Literal, Module, Operator, Parameter, Type,
  Variant,
};
use crate::{
  error::{Error, ParserError},
  lexer::token::{Token, TokenKind},
};
use std::{iter::Peekable, slice::Iter};

macro_rules! list {
  ($self:ident, $end:pat, $separator:pat, $parse:expr) => {{
    let mut items = Vec::new();

    match $self.tokens.peek() {
      Some(&(_, $end)) => {
        $self.tokens.next();
      }
      Some(_) => {
        items.push($parse($self)?);
        loop {
          match $self.tokens.peek().copied() {
            Some(&(_, $separator)) => {
              $self.tokens.next();
              items.push($parse($self)?);
            }
            Some(&(_, $end)) => {
              $self.tokens.next();
              break;
            }
            token => Err($self.unexpected_token(token))?,
          }
        }
      }
      None => Err($self.error(None, ParserError::UnexpectedToken(None)))?,
    }

    items
  }};
}

pub struct Parser<'a> {
  pub tokens: Peekable<Iter<'a, Token>>,
}

impl Parser<'_> {
  pub fn new(tokens: Peekable<Iter<Token>>) -> Parser {
    Parser { tokens }
  }

  // TODO: handle a none value properly
  pub fn error(&self, token: Option<&Token>, error: ParserError) -> Error<ParserError> {
    Error(token.map_or(0..0, |t| t.0.clone()), error)
  }

  pub fn unexpected_token(&self, token: Option<&Token>) -> Error<ParserError> {
    self.error(
      token,
      ParserError::UnexpectedToken(token.map(|t| t.1.clone())),
    )
  }

  pub fn parse(&mut self) -> Result<Module, Error<ParserError>> {
    let mut items = Vec::new();

    loop {
      match self.tokens.peek() {
        None => break,
        Some(&token) => {
          let public = token.1 == TokenKind::Pub;
          match if public {
            self.tokens.next()
          } else {
            Some(token)
          } {
            Some((_, TokenKind::Eof)) | None => {
              if public {
                Err(self.unexpected_token(Some(token)))?
              } else {
                break;
              }
            }
            Some(_) => items.push((public, self.parse_item()?)),
          }
        }
      }
    }

    Ok(Module { items })
  }

  fn parse_item(&mut self) -> Result<Item, Error<ParserError>> {
    Ok(match self.tokens.peek().copied() {
      Some((_, TokenKind::Fn)) => Item::Function(self.parse_function()?),
      Some((_, TokenKind::Enum)) => Item::Enum(self.parse_enum()?),
      Some((_, TokenKind::Op)) => Item::Operator(self.parse_operator()?),
      token => Err(self.unexpected_token(token))?,
    })
  }

  fn parse_function(&mut self) -> Result<Function, Error<ParserError>> {
    match self.tokens.next() {
      Some((_, TokenKind::Fn)) => {}
      token => Err(self.unexpected_token(token))?,
    }

    let name = match self.tokens.next() {
      Some((_, TokenKind::Identifier(name))) => name,
      token => Err(self.unexpected_token(token))?,
    }
    .to_string();

    match self.tokens.next() {
      Some((_, TokenKind::LeftParen)) => {}
      token => Err(self.unexpected_token(token))?,
    }

    let parameters = list!(
      self,
      TokenKind::RightParen,
      TokenKind::Comma,
      Parser::parse_parameter
    );

    let return_type = match self.tokens.peek() {
      Some((_, TokenKind::Operator(operator))) if operator == "->" => {
        self.tokens.next();
        Some(self.parse_type()?)
      }
      _ => None,
    }
    .unwrap_or(Type::Tuple(Vec::new()));

    let body = self.parse_expression()?;

    Ok(Function {
      name,
      parameters,
      return_type,
      body,
    })
  }

  fn parse_parameter(&mut self) -> Result<Parameter, Error<ParserError>> {
    let name = match self.tokens.next() {
      Some((_, TokenKind::Identifier(name))) => name,
      token => Err(self.unexpected_token(token))?,
    }
    .to_string();

    match self.tokens.next() {
      Some((_, TokenKind::Operator(operator))) if operator == ":" => {}
      token => Err(self.unexpected_token(token))?,
    }

    let ty = self.parse_type()?;

    Ok(Parameter { name, ty })
  }

  fn parse_expression(&mut self) -> Result<Expression, Error<ParserError>> {
    let mut expression = match self.tokens.next() {
      Some((_, TokenKind::Identifier(name))) => Ok(Expression::Identifier(name.clone())),
      Some((_, TokenKind::If)) => {
        let condition = Box::new(self.parse_expression()?);
        let then = Box::new(self.parse_expression()?);
        let otherwise = match self.tokens.peek() {
          Some((_, TokenKind::Else)) => {
            self.tokens.next();
            Some(Box::new(self.parse_expression()?))
          }
          _ => None,
        };

        Ok(Expression::If(condition, then, otherwise))
      }
      Some((_, TokenKind::While)) => {
        let condition = Box::new(self.parse_expression()?);
        let body = Box::new(self.parse_expression()?);

        Ok(Expression::While(condition, body))
      }
      Some((_, TokenKind::Return)) => Ok(Expression::Return(Box::new(self.parse_expression()?))),
      Some((_, TokenKind::Let)) => {
        let name = match self.tokens.next() {
          Some((_, TokenKind::Identifier(name))) => name,
          token => Err(self.unexpected_token(token))?,
        }
        .to_string();

        Ok(match self.tokens.next() {
          Some((_, TokenKind::Operator(operator))) if operator == "=" => {
            let value = Box::new(self.parse_expression()?);
            Expression::DeclarationAssignment(name, value)
          }
          _ => Expression::Declaration(name),
        })
      }
      Some((_, TokenKind::LeftBrace)) => {
        let mut ended = false;
        let mut value = false;
        let mut expressions = Vec::new();

        loop {
          match self.tokens.peek().copied() {
            Some((_, TokenKind::RightBrace)) => {
              self.tokens.next();
              break;
            }
            token => {
              if ended {
                Err(self.unexpected_token(token))?;
              }

              value = true;
              expressions.push(self.parse_expression()?);

              match self.tokens.peek().copied() {
                Some((_, TokenKind::Semicolon)) => {
                  self.tokens.next();
                  value = false;
                }
                _ => {
                  ended = true;
                }
              }
            }
          }
        }

        Ok(Expression::Block(expressions, value))
      }
      Some((_, TokenKind::StringLiteral(value))) => {
        Ok(Expression::Literal(Literal::String(value.clone())))
      }
      Some((_, TokenKind::LeftParen)) => Ok(Expression::Literal(Literal::Tuple(list!(
        self,
        TokenKind::RightParen,
        TokenKind::Comma,
        Parser::parse_expression
      )))),
      Some((_, TokenKind::LeftBracket)) => Ok(Expression::Literal(Literal::Array(list!(
        self,
        TokenKind::RightBracket,
        TokenKind::Comma,
        Parser::parse_expression
      )))),
      token => Err(self.unexpected_token(token))?,
    }?;

    // TODO: better way to handle this?
    loop {
      match self.tokens.peek() {
        Some((_, TokenKind::LeftParen)) => {
          self.tokens.next();
          expression = Expression::Call(
            Box::new(expression),
            list!(
              self,
              TokenKind::RightParen,
              TokenKind::Comma,
              Parser::parse_expression
            ),
          )
        }
        Some((_, TokenKind::Operator(operator))) => match operator.as_str() {
          "." => {
            self.tokens.next();
            expression = Expression::Field(
              Box::new(expression),
              match self.tokens.next() {
                Some((_, TokenKind::Identifier(name))) => FieldAccess::Identifier(name.clone()),
                token => Err(self.unexpected_token(token))?,
              },
            )
          }
          "[" => {
            self.tokens.next();
            expression =
              Expression::Index(Box::new(expression), Box::new(self.parse_expression()?));

            match self.tokens.next() {
              Some((_, TokenKind::RightBracket)) => {}
              token => Err(self.unexpected_token(token))?,
            }
          }
          "=" => {
            self.tokens.next();
            expression =
              Expression::Assignment(Box::new(expression), Box::new(self.parse_expression()?))
          }
          _ => break,
        },
        _ => break,
      };
    }

    Ok(expression)
  }

  fn parse_enum(&mut self) -> Result<Enum, Error<ParserError>> {
    match self.tokens.next() {
      Some((_, TokenKind::Enum)) => {}
      token => Err(self.unexpected_token(token))?,
    }

    let name = match self.tokens.next() {
      Some((_, TokenKind::Identifier(name))) => name,
      token => Err(self.unexpected_token(token))?,
    }
    .to_string();

    match self.tokens.next() {
      Some((_, TokenKind::LeftBrace)) => {}
      token => Err(self.unexpected_token(token))?,
    }

    let variants = list!(
      self,
      TokenKind::RightBrace,
      TokenKind::Comma,
      Parser::parse_variant
    );

    Ok(Enum { name, variants })
  }

  // operator overloading
  /*

  e.g.

  op + (a: i32, b: i32): i32 {
    a + b
  }

  op ~ (a: bool): bool {
    !a
  }

  */
  fn parse_operator(&mut self) -> Result<Operator, Error<ParserError>> {
    match self.tokens.next() {
      Some((_, TokenKind::Op)) => {}
      token => Err(self.unexpected_token(token))?,
    }

    let operator = match self.tokens.next() {
      Some((_, TokenKind::Operator(operator))) => operator,
      token => Err(self.unexpected_token(token))?,
    }
    .to_string();

    match self.tokens.next() {
      Some((_, TokenKind::LeftParen)) => {}
      token => Err(self.unexpected_token(token))?,
    }

    let a = self.parse_parameter()?;

    Ok(match self.tokens.peek().copied() {
      Some((_, TokenKind::Comma)) => {
        self.tokens.next();

        let b = self.parse_parameter()?;

        match self.tokens.next() {
          Some((_, TokenKind::RightParen)) => {}
          token => Err(self.unexpected_token(token))?,
        }

        match self.tokens.next() {
          Some((_, TokenKind::Operator(operator))) if operator == "->" => {}
          token => Err(self.unexpected_token(token))?,
        }

        Operator::Infix {
          operator,
          operands: (a, b),
          result: self.parse_type()?,
          body: self.parse_expression()?,
        }
      }
      _ => {
        match self.tokens.next() {
          Some((_, TokenKind::RightParen)) => {}
          token => Err(self.unexpected_token(token))?,
        }

        match self.tokens.next() {
          Some((_, TokenKind::Operator(operator))) if operator == "->" => {}
          token => Err(self.unexpected_token(token))?,
        }

        Operator::Prefix {
          operator,
          operand: a,
          result: self.parse_type()?,
          body: self.parse_expression()?,
        }
      }
    })
  }

  fn parse_variant(&mut self) -> Result<Variant, Error<ParserError>> {
    let name = match self.tokens.next() {
      Some((_, TokenKind::Identifier(name))) => name,
      token => Err(self.unexpected_token(token))?,
    }
    .to_string();

    let fields = match self.tokens.peek() {
      Some((_, TokenKind::LeftParen)) => {
        self.tokens.next();
        list!(
          self,
          TokenKind::RightParen,
          TokenKind::Comma,
          Parser::parse_type
        )
      }
      _ => Vec::new(),
    };

    Ok(Variant { name, fields })
  }

  fn parse_type(&mut self) -> Result<Type, Error<ParserError>> {
    let ty = match self.tokens.next() {
      Some(token) => Ok(match &token.1 {
        TokenKind::Identifier(name) => Type::Named(name.to_string()),

        TokenKind::Char => Type::Char,
        TokenKind::I8 => Type::I8,
        TokenKind::I16 => Type::I16,
        TokenKind::I32 => Type::I32,
        TokenKind::I64 => Type::I64,
        TokenKind::I128 => Type::I128,
        TokenKind::U8 => Type::U8,
        TokenKind::U16 => Type::U16,
        TokenKind::U32 => Type::U32,
        TokenKind::U64 => Type::U64,
        TokenKind::U128 => Type::U128,
        TokenKind::F16 => Type::F16,
        TokenKind::F32 => Type::F32,
        TokenKind::F64 => Type::F64,
        TokenKind::F128 => Type::F128,

        TokenKind::LeftParen => {
          let list = list!(
            self,
            TokenKind::RightParen,
            TokenKind::Comma,
            Parser::parse_type
          );

          match self.tokens.peek() {
            // TODO: are there cases where this may cause ambiguity?
            Some((_, TokenKind::Operator(operator))) => match operator.as_str() {
              ":" => {
                self.tokens.next();
                Type::Function(list, Box::new(self.parse_type()?))
              }
              _ => Type::Tuple(list),
            },

            _ => Type::Tuple(list),
          }
        }

        TokenKind::LeftBracket => {
          let ty = Box::new(self.parse_type()?);
          match self.tokens.next() {
            Some((_, TokenKind::RightBracket)) => Type::Array(ty),
            token => Err(self.unexpected_token(token))?,
          }
        }

        _ => Err(self.unexpected_token(Some(token)))?,
      }),
      None => Err(self.unexpected_token(None))?,
    };

    if let Some(&(_, TokenKind::Operator(operator))) = self.tokens.peek() {
      match operator.as_str() {
        "|" => {
          self.tokens.next();
          Ok(Type::Union(Box::new(ty?), Box::new(self.parse_type()?)))
        }
        _ => ty,
      }
    } else {
      ty
    }
  }
}
