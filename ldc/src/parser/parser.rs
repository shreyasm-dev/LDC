use super::ast::{
  Enum, Expression, Function, Item, Literal, Module, Operator, Parameter, Type, Variant,
};
use crate::{
  error::{Error, ParserError},
  lexer::token::{Token, TokenKind},
};
use std::{iter::Peekable, slice::Iter};

macro_rules! list {
  ($self:ident, $end:path, $separator:path, $parse:expr) => {{
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
            token => Err($self.unexpected_token(token, vec![$separator, $end]))?,
          }
        }
      }
      None => Err($self.error(
        None,
        ParserError::UnexpectedToken(None, vec![$separator, $end]),
      ))?,
    }

    items
  }};
}

macro_rules! expect {
  ($self:ident, $next:expr, $($token:path => $block:block),*) => {
    match if $next { $self.tokens.next() } else { $self.tokens.peek().copied() } {
      $(Some((_, $token)) => Ok($block),)*
      token => Err($self.unexpected_token(token, vec![$($token),*])),
    }
  }
}

// https://news.ycombinator.com/item?id=13915458
macro_rules! operator {
  ($self:ident, $operator:expr, $expression:expr, $precedence:expr) => {
    let op = TokenKind::Operator($operator.to_string());
    $expression = if op.precedence() >= $precedence {
      $self.tokens.next();
      Expression::Infix(
        Box::new($expression),
        $operator.to_string(),
        Box::new($self.parse_expression_with_precedence(
          op.precedence() + if op.left_associative() { 1 } else { 0 },
        )?),
      )
    } else {
      break;
    }
  };
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

  pub fn unexpected_token(
    &self,
    token: Option<&Token>,
    expected: Vec<TokenKind>,
  ) -> Error<ParserError> {
    self.error(
      token,
      ParserError::UnexpectedToken(token.map(|t| t.1.clone()), expected),
    )
  }

  pub fn expect(&mut self, expected: Vec<TokenKind>) -> Result<Token, Error<ParserError>> {
    match self.tokens.next() {
      Some(token) => {
        if expected.contains(&token.1) {
          Ok(token.clone())
        } else {
          Err(self.unexpected_token(Some(token), expected))
        }
      }
      None => Err(self.unexpected_token(None, expected)),
    }
  }

  pub fn expect_operator(&mut self, operators: Vec<&str>) -> Result<String, Error<ParserError>> {
    let expected = operators
      .iter()
      .map(|&o| TokenKind::Operator(o.to_string()))
      .collect::<Vec<_>>();

    match self.tokens.next() {
      Some((span, TokenKind::Operator(operator))) => {
        if operators.contains(&operator.as_str()) {
          Ok(operator.to_string())
        } else {
          Err(self.unexpected_token(
            Some(&(span.clone(), TokenKind::Operator(operator.to_string()))),
            expected,
          ))?
        }
      }
      token => Err(self.unexpected_token(token, expected))?,
    }
  }

  pub fn expect_identifier(&mut self) -> Result<String, Error<ParserError>> {
    match self.tokens.next() {
      Some((_, TokenKind::Identifier(name))) => Ok(name.to_string()),
      token => Err(self.unexpected_token(token, vec![TokenKind::Identifier("".to_string())]))?,
    }
  }

  pub fn parse(&mut self) -> Result<Module, Error<ParserError>> {
    let mut items = Vec::new();

    loop {
      match self.tokens.peek() {
        None | Some((_, TokenKind::Eof)) => break,
        Some(&token) => {
          let public = token.1 == TokenKind::Pub;
          if public {
            self.tokens.next();
          }

          items.push((public, self.parse_item()?));
        }
      }
    }

    Ok(Module { items })
  }

  fn parse_item(&mut self) -> Result<Item, Error<ParserError>> {
    expect! {
      self,
      false,
      TokenKind::Fn => { Item::Function(self.parse_function()?) },
      TokenKind::Enum => { Item::Enum(self.parse_enum()?) },
      TokenKind::Op => { Item::Operator(self.parse_operator()?) }
    }
  }

  fn parse_function(&mut self) -> Result<Function, Error<ParserError>> {
    self.expect(vec![TokenKind::Fn])?;

    let name = self.expect_identifier()?;

    self.expect(vec![TokenKind::LeftParen])?;

    let parameters = list!(
      self,
      TokenKind::RightParen,
      TokenKind::Comma,
      Parser::parse_parameter
    );

    let return_type = self.parse_return_type(true)?;
    let body = self.parse_expression()?;

    Ok(Function {
      name,
      parameters,
      return_type,
      body,
    })
  }

  fn parse_parameter(&mut self) -> Result<Parameter, Error<ParserError>> {
    let name = self.expect_identifier()?;

    self.expect_operator(vec![":"])?;

    let ty = self.parse_type()?;

    Ok(Parameter { name, ty })
  }

  fn parse_expression(&mut self) -> Result<Expression, Error<ParserError>> {
    self.parse_expression_with_precedence(0)
  }

  fn parse_expression_with_precedence(
    &mut self,
    precedence: u8,
  ) -> Result<Expression, Error<ParserError>> {
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
                Err(
                  self.unexpected_token(token, vec![TokenKind::Semicolon, TokenKind::RightBrace]),
                )?;
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
      Some((_, TokenKind::Fn)) => {
        self.expect(vec![TokenKind::LeftParen])?;

        let parameters = list!(
          self,
          TokenKind::RightParen,
          TokenKind::Comma,
          Parser::parse_parameter
        );

        let return_type = self.parse_return_type(true)?;
        let body = self.parse_expression()?;

        Ok(Expression::Literal(Literal::Closure(
          parameters,
          return_type,
          Box::new(body),
        )))
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
      token => Err(self.unexpected_token(
        token,
        vec![
          TokenKind::Identifier("".to_string()),
          TokenKind::If,
          TokenKind::While,
          TokenKind::Return,
          TokenKind::Let,
          TokenKind::LeftBrace,
          TokenKind::StringLiteral("".to_string()),
          TokenKind::Fn,
          TokenKind::LeftParen,
          TokenKind::LeftBracket,
        ],
      ))?,
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
        Some((_, TokenKind::Operator(operator))) => {
          operator!(self, operator, expression, precedence);

          loop {
            match self.tokens.peek() {
              Some((_, TokenKind::Operator(operator))) => {
                operator!(self, operator, expression, precedence);
              }
              _ => break,
            }
          }
        }
        Some((_, TokenKind::LeftBracket)) => {
          self.tokens.next();
          expression = Expression::Index(Box::new(expression), Box::new(self.parse_expression()?));
          self.expect(vec![TokenKind::RightBracket])?;
        }
        _ => break,
      };
    }

    Ok(expression)
  }

  fn parse_enum(&mut self) -> Result<Enum, Error<ParserError>> {
    self.expect(vec![TokenKind::Enum])?;

    let name = self.expect_identifier()?;

    self.expect(vec![TokenKind::LeftBrace])?;

    let variants = list!(
      self,
      TokenKind::RightBrace,
      TokenKind::Comma,
      Parser::parse_variant
    );

    Ok(Enum { name, variants })
  }

  fn parse_operator(&mut self) -> Result<Operator, Error<ParserError>> {
    self.expect(vec![TokenKind::Op])?;

    let operator = match self.tokens.next() {
      Some((_, TokenKind::Operator(operator))) => operator,
      token => Err(self.unexpected_token(token, vec![TokenKind::Operator("".to_string())]))?,
    }
    .to_string();

    self.expect(vec![TokenKind::LeftParen])?;

    let a = self.parse_parameter()?;
    let b = match self.tokens.peek() {
      Some((_, TokenKind::Comma)) => {
        self.tokens.next();
        Some(self.parse_parameter()?)
      }
      _ => None,
    };

    self.expect(vec![TokenKind::RightParen])?;

    let result = self.parse_return_type(false)?.unwrap();
    let body = self.parse_expression()?;

    Ok(match b {
      Some(b) => Operator::Infix {
        operator,
        operands: (a, b),
        result,
        body,
      },
      None => Operator::Prefix {
        operator,
        operand: a,
        result,
        body,
      },
    })
  }

  fn parse_variant(&mut self) -> Result<Variant, Error<ParserError>> {
    let name = self.expect_identifier()?;

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
    let expected = vec![
      TokenKind::Identifier("".to_string()),
      TokenKind::Char,
      TokenKind::I8,
      TokenKind::I16,
      TokenKind::I32,
      TokenKind::I64,
      TokenKind::I128,
      TokenKind::U8,
      TokenKind::U16,
      TokenKind::U32,
      TokenKind::U64,
      TokenKind::U128,
      TokenKind::F16,
      TokenKind::F32,
      TokenKind::F64,
      TokenKind::F128,
      TokenKind::LeftParen,
      TokenKind::LeftBracket,
    ];

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
          self.expect(vec![TokenKind::RightBracket])?;
          Type::Array(ty)
        }
        TokenKind::Fn => {
          self.expect(vec![TokenKind::LeftParen])?;

          let parameters = list!(
            self,
            TokenKind::RightParen,
            TokenKind::Comma,
            Parser::parse_type
          );
          let return_type = self.parse_return_type(false)?.unwrap();

          Type::Function(parameters, Box::new(return_type))
        }
        _ => Err(self.unexpected_token(Some(token), expected))?,
      }),
      None => Err(self.unexpected_token(None, expected))?,
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

  fn parse_return_type(&mut self, optional: bool) -> Result<Option<Type>, Error<ParserError>> {
    match self.tokens.peek().copied() {
      Some((_, TokenKind::Operator(operator))) if operator == ":" => {
        self.tokens.next();
        Ok(Some(self.parse_type()?))
      }
      token => {
        if optional {
          Ok(None)
        } else {
          Err(self.unexpected_token(token, vec![TokenKind::Operator(":".to_string())]))
        }
      }
    }
  }
}
