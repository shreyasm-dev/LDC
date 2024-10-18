use super::ast::{
  Enum, EnumHeader, Expression, Function, FunctionHeader, Header, Item, Literal, Module, Operator,
  OperatorHeader, Parameter, Struct, StructHeader, Trait, TraitHeader, Type, Variant,
};
use crate::{
  error::{Error, ParserError},
  lexer::token::{Token, TokenKind},
};
use std::{iter::Peekable, slice::Iter};

macro_rules! expect {
  ($self:ident, $next:expr, $($token:pat = $value:expr => $block:block),*) => {
    match if $next { $self.tokens.next() } else { $self.tokens.peek().copied() } {
      $(Some((_, $token)) => Ok($block),)*
      token => Err($self.unexpected_token(token, vec![$($value),*])),
    }
  }
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

  pub fn expect_list<T, F>(
    &mut self,
    end: TokenKind,
    separator: TokenKind,
    parse: F,
  ) -> Result<Vec<T>, Error<ParserError>>
  where
    F: Fn(&mut Parser) -> Result<T, Error<ParserError>>,
  {
    let expected = vec![separator.clone(), end.clone()];
    let mut items = Vec::new();

    match self.tokens.peek().copied() {
      Some(&(_, ref token)) if *token == end => {
        self.tokens.next();
      }
      Some(_) => {
        items.push(parse(self)?);
        loop {
          match self.tokens.peek().cloned() {
            Some(&(_, ref token)) if *token == separator => {
              self.tokens.next();
              items.push(parse(self)?);
            }
            Some(&(_, ref token)) if *token == end => {
              self.tokens.next();
              break;
            }
            token => Err(self.unexpected_token(token, expected.clone()))?,
          }
        }
      }
      None => Err(self.error(None, ParserError::UnexpectedToken(None, expected)))?,
    }

    Ok(items)
  }

  pub fn expect_identifier_list(
    &mut self,
    separator: TokenKind,
  ) -> Result<Vec<String>, Error<ParserError>> {
    let mut items = Vec::new();

    match self.tokens.peek().copied() {
      Some((_, TokenKind::Identifier(name))) => {
        items.push(name.to_string());
        self.tokens.next();
      }
      token => Err(self.unexpected_token(token, vec![TokenKind::Identifier("".to_string())]))?,
    }

    loop {
      match self.tokens.peek().copied() {
        Some((_, token)) if *token == separator => {
          self.tokens.next();
          match self.tokens.next() {
            Some((_, TokenKind::Identifier(name))) => items.push(name.to_string()),
            token => {
              Err(self.unexpected_token(token, vec![TokenKind::Identifier("".to_string())]))?
            }
          }
        }
        _ => break,
      }
    }

    Ok(items)
  }

  pub fn parse(&mut self) -> Result<Module, Error<ParserError>> {
    let mut items = Vec::new();

    loop {
      match self.tokens.peek() {
        None | Some((_, TokenKind::Eof)) => break,
        _ => match self.parse_item(false)? {
          (public, _, item) => items.push((public, item)),
        },
      }
    }

    Ok(Module { items })
  }

  fn parse_item(&mut self, allow_static: bool) -> Result<(bool, bool, Item), Error<ParserError>> {
    let public = match self.tokens.peek() {
      Some((_, TokenKind::Pub)) => {
        self.tokens.next();
        true
      }
      _ => false,
    };

    let static_ = if allow_static {
      match self.tokens.peek() {
        Some((_, TokenKind::Static)) => {
          self.tokens.next();
          true
        }
        _ => false,
      }
    } else {
      false
    };

    let value = expect! {
      self,
      false,
      TokenKind::Fn = TokenKind::Fn => { Item::Function(self.parse_function()?) },
      TokenKind::Struct = TokenKind::Struct => { Item::Struct(self.parse_struct()?) },
      TokenKind::Enum = TokenKind::Enum => { Item::Enum(self.parse_enum()?) },
      TokenKind::Trait = TokenKind::Trait => { Item::Trait(self.parse_trait()?) },
      TokenKind::Operator(_) = TokenKind::Operator("".to_string()) => { Item::Operator(self.parse_operator()?) }
    }?;

    self.expect(vec![TokenKind::Semicolon])?;

    Ok((public, static_, value))
  }

  fn parse_header(
    &mut self,
    allow_static: bool,
    named: bool,
  ) -> Result<(bool, bool, Header), Error<ParserError>> {
    let public = match self.tokens.peek() {
      Some((_, TokenKind::Pub)) => {
        self.tokens.next();
        true
      }
      _ => false,
    };

    let static_ = if allow_static {
      match self.tokens.peek() {
        Some((_, TokenKind::Static)) => {
          self.tokens.next();
          true
        }
        _ => false,
      }
    } else {
      false
    };

    let value = expect! {
      self,
      false,
      TokenKind::Fn = TokenKind::Fn => { Header::Function(self.parse_function_header(named)?) },
      TokenKind::Struct = TokenKind::Struct => { Header::Struct(self.parse_struct_header()?) },
      TokenKind::Enum = TokenKind::Enum => { Header::Enum(self.parse_enum_header()?) },
      TokenKind::Trait = TokenKind::Trait => { Header::Trait(self.parse_trait_header()?) },
      TokenKind::Operator(_) = TokenKind::Operator("".to_string()) => { Header::Operator(self.parse_operator_header(named)?) }
    }?;

    self.expect(vec![TokenKind::Semicolon])?;

    Ok((public, static_, value))
  }

  fn parse_function(&mut self) -> Result<Function, Error<ParserError>> {
    let header = self.parse_function_header(true)?;
    let body = self.parse_expression()?;

    Ok(Function { header, body })
  }

  fn parse_struct(&mut self) -> Result<Struct, Error<ParserError>> {
    let header = self.parse_struct_header()?;

    self.expect(vec![TokenKind::LeftBrace])?;

    let mut items = Vec::new();

    loop {
      match self.tokens.peek() {
        Some((_, TokenKind::RightBrace)) => {
          self.tokens.next();
          break;
        }
        _ => items.push(self.parse_item(true)?),
      }
    }

    Ok(Struct { header, items })
  }

  fn parse_struct_header(&mut self) -> Result<StructHeader, Error<ParserError>> {
    self.expect(vec![TokenKind::Struct])?;

    let name = self.expect_identifier()?;

    let traits = match self.tokens.peek() {
      Some((_, TokenKind::Operator(operator))) if operator == ":" => {
        self.tokens.next();
        self.expect_identifier_list(TokenKind::Comma)?
      }
      _ => Vec::new(),
    };

    Ok(StructHeader { name, traits })
  }

  fn parse_function_header(&mut self, named: bool) -> Result<FunctionHeader, Error<ParserError>> {
    self.expect(vec![TokenKind::Fn])?;

    let name = self.expect_identifier()?;

    self.expect(vec![TokenKind::LeftParen])?;

    let parameters = self.expect_list(TokenKind::RightParen, TokenKind::Comma, |parser| {
      if named {
        parser.parse_parameter()
      } else {
        Ok(Parameter {
          name: "".to_string(),
          ty: parser.parse_type()?,
        })
      }
    })?;

    let return_type = self.parse_type_annotation(true)?;

    Ok(FunctionHeader {
      name,
      parameters,
      return_type,
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
      Some((_, TokenKind::Operator(operator))) => {
        let op = TokenKind::Operator(operator.to_string());
        Ok(Expression::Prefix(
          operator.to_string(),
          Box::new(self.parse_expression_with_precedence(op.prefix_precedence())?),
        ))
      }
      Some((_, TokenKind::Identifier(name))) => Ok(Expression::Identifier(name.clone())),
      Some((_, TokenKind::Let)) => Ok(Expression::Declaration(self.expect_identifier()?)),
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
      Some((_, TokenKind::CharLiteral(value))) => {
        Ok(Expression::Literal(Literal::Char(value.clone())))
      }
      Some((_, TokenKind::Fn)) => {
        self.expect(vec![TokenKind::LeftParen])?;

        let parameters = self.expect_list(TokenKind::RightParen, TokenKind::Comma, |parser| {
          parser.parse_parameter()
        })?;

        let return_type = self.parse_type_annotation(true)?;
        let body = self.parse_expression()?;

        Ok(Expression::Literal(Literal::Closure(
          parameters,
          return_type,
          Box::new(body),
        )))
      }
      Some((_, TokenKind::LeftParen)) => Ok(Expression::Literal(Literal::Tuple(
        self.expect_list(TokenKind::RightParen, TokenKind::Comma, |parser| {
          parser.parse_expression()
        })?,
      ))),
      Some((_, TokenKind::LeftBracket)) => Ok(Expression::Literal(Literal::Array(
        self.expect_list(TokenKind::RightBracket, TokenKind::Comma, |parser| {
          parser.parse_expression()
        })?,
      ))),
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

    loop {
      match self.tokens.peek() {
        // calling and indexing have the highest precedence
        Some((_, TokenKind::LeftParen)) => {
          self.tokens.next();
          expression = Expression::Call(
            Box::new(expression),
            self.expect_list(TokenKind::RightParen, TokenKind::Comma, |parser| {
              parser.parse_expression()
            })?,
          )
        }
        Some((_, TokenKind::LeftBracket)) => {
          self.tokens.next();
          expression = Expression::Index(Box::new(expression), Box::new(self.parse_expression()?));
          self.expect(vec![TokenKind::RightBracket])?;
        }
        Some((_, TokenKind::Operator(operator))) => {
          // https://news.ycombinator.com/item?id=13915458
          // TODO: x == y == z etc
          let op = TokenKind::Operator(operator.to_string());
          expression = if op.infix_precedence() >= precedence {
            self.tokens.next();
            Expression::Infix(
              Box::new(expression),
              operator.to_string(),
              Box::new(self.parse_expression_with_precedence(
                op.infix_precedence() + if op.left_associative() { 1 } else { 0 },
              )?),
            )
          } else {
            break;
          }
        }
        _ => break,
      };
    }

    Ok(expression)
  }

  fn parse_enum(&mut self) -> Result<Enum, Error<ParserError>> {
    let header = self.parse_enum_header()?;

    self.expect(vec![TokenKind::LeftBrace])?;

    let variants = self.expect_list(TokenKind::RightBrace, TokenKind::Comma, |parser| {
      parser.parse_variant()
    })?;

    Ok(Enum { header, variants })
  }

  fn parse_enum_header(&mut self) -> Result<EnumHeader, Error<ParserError>> {
    self.expect(vec![TokenKind::Enum])?;

    let name = self.expect_identifier()?;

    Ok(EnumHeader { name })
  }

  fn parse_trait(&mut self) -> Result<Trait, Error<ParserError>> {
    let header = self.parse_trait_header()?;

    self.expect(vec![TokenKind::LeftBrace])?;

    let mut items = Vec::new();

    loop {
      match self.tokens.peek() {
        Some((_, TokenKind::RightBrace)) => {
          self.tokens.next();
          break;
        }
        _ => items.push(self.parse_header(true, false)?),
      }
    }

    Ok(Trait { header, items })
  }

  fn parse_trait_header(&mut self) -> Result<TraitHeader, Error<ParserError>> {
    self.expect(vec![TokenKind::Trait])?;

    let name = self.expect_identifier()?;

    let traits = match self.tokens.peek() {
      Some((_, TokenKind::Operator(operator))) if operator == ":" => {
        self.tokens.next();
        self.expect_identifier_list(TokenKind::Comma)?
      }
      _ => Vec::new(),
    };

    Ok(TraitHeader { name, traits })
  }

  fn parse_operator(&mut self) -> Result<Operator, Error<ParserError>> {
    let header = self.parse_operator_header(true)?;
    let body = self.parse_expression()?;

    Ok(Operator { header, body })
  }

  fn parse_operator_header(&mut self, named: bool) -> Result<OperatorHeader, Error<ParserError>> {
    let operator = match self.tokens.next() {
      Some((_, TokenKind::Operator(operator))) => operator,
      token => Err(self.unexpected_token(token, vec![TokenKind::Operator("".to_string())]))?,
    }
    .to_string();

    self.expect(vec![TokenKind::LeftParen])?;

    let a = if named {
      self.parse_parameter()?
    } else {
      Parameter {
        name: "".to_string(),
        ty: self.parse_type()?,
      }
    };
    let b = match self.tokens.peek() {
      Some((_, TokenKind::Comma)) => {
        self.tokens.next();
        Some(if named {
          self.parse_parameter()?
        } else {
          Parameter {
            name: "".to_string(),
            ty: self.parse_type()?,
          }
        })
      }
      _ => None,
    };

    self.expect(vec![TokenKind::RightParen])?;

    let result = self.parse_type_annotation(false)?.unwrap();

    Ok(match b {
      Some(b) => OperatorHeader::Infix {
        operator,
        operands: (a.ty, b.ty),
        result,
      },
      None => OperatorHeader::Prefix {
        operator,
        operand: a.ty,
        result,
      },
    })
  }

  fn parse_variant(&mut self) -> Result<Variant, Error<ParserError>> {
    let name = self.expect_identifier()?;

    let fields = match self.tokens.peek() {
      Some((_, TokenKind::LeftParen)) => {
        self.tokens.next();
        self.expect_list(TokenKind::RightParen, TokenKind::Comma, |parser| {
          parser.parse_type()
        })?
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
          let list = self.expect_list(TokenKind::RightParen, TokenKind::Comma, |parser| {
            parser.parse_type()
          })?;

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

          let parameters = self.expect_list(TokenKind::RightParen, TokenKind::Comma, |parser| {
            parser.parse_type()
          })?;
          let return_type = self.parse_type_annotation(false)?.unwrap();

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

  fn parse_type_annotation(&mut self, optional: bool) -> Result<Option<Type>, Error<ParserError>> {
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
