use super::ast::*;
use crate::{
  error::{Error, ParserError},
  expect,
  lexer::token::{NumericType, Token, TokenKind},
};
use std::{collections::BTreeSet, iter::Peekable, slice::Iter};

type Type = util::Type<String>;

#[derive(Debug, Clone)]
pub struct Parser<'a> {
  pub tokens: Peekable<Iter<'a, Token>>,
}

impl Parser<'_> {
  pub fn new(tokens: Peekable<Iter<Token>>) -> Parser {
    Parser { tokens }
  }

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

  pub fn expect_list_without_end<T, F>(
    &mut self,
    separator: TokenKind,
    parse: F,
  ) -> Result<Vec<T>, Error<ParserError>>
  where
    F: Fn(&mut Parser) -> Result<T, Error<ParserError>>,
  {
    let mut clone = self.clone();
    let mut items = Vec::new();

    match clone.tokens.peek().copied() {
      Some(_) => {
        items.push(parse(&mut clone)?);
        loop {
          match clone.tokens.peek().cloned() {
            Some((_, ref token)) if *token == separator => {
              clone.tokens.next();
              match parse(&mut clone) {
                Ok(item) => items.push(item),
                Err(_) => break,
              }
            }
            _ => break,
          }
        }
      }
      None => (),
    }

    std::mem::swap(&mut self.tokens, &mut clone.tokens);

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

  pub fn parse(&mut self) -> Result<module::Module<Type>, Error<ParserError>> {
    let mut items = Vec::new();

    loop {
      match self.tokens.peek() {
        None | Some((_, TokenKind::Eof)) => break,
        _ => items.push(self.parse_item(false)?),
      }
    }

    Ok(module::Module { items })
  }

  pub fn parse_modifiers(&mut self, allow_static: bool) -> util::Modifiers {
    let mut modifiers = util::Modifiers::default();

    loop {
      match self.tokens.peek() {
        Some((_, TokenKind::Pub)) => {
          self.tokens.next();
          modifiers.public = true;
        }
        Some((_, TokenKind::Static)) if allow_static => {
          self.tokens.next();
          modifiers.static_ = true;
        }
        _ => break,
      }
    }

    modifiers
  }

  fn parse_item(&mut self, allow_static: bool) -> Result<module::Item<Type>, Error<ParserError>> {
    let modifiers = self.parse_modifiers(allow_static);

    let kind = expect! {
      self,
      false,
      TokenKind::Fn = TokenKind::Fn => { module::ItemKind::Function(self.parse_function()?) },
      TokenKind::Struct = TokenKind::Struct => { module::ItemKind::Struct(self.parse_struct()?) },
      TokenKind::Enum = TokenKind::Enum => { module::ItemKind::Enum(self.parse_enum()?) },
      TokenKind::Trait = TokenKind::Trait => { module::ItemKind::Trait(self.parse_trait()?) },
      TokenKind::Operator(_) = TokenKind::Operator("".to_string()) => { module::ItemKind::Operator(self.parse_operator()?) }
    }?;

    self.expect(vec![TokenKind::Semicolon])?;

    Ok(module::Item { modifiers, kind })
  }

  fn parse_function(&mut self) -> Result<function::Function<Type>, Error<ParserError>> {
    self.expect(vec![TokenKind::Fn])?;

    let name = self.expect_identifier()?;
    let type_parameters = self.parse_type_parameter()?;

    self.expect(vec![TokenKind::LeftParen])?;

    let parameters = self.expect_list(TokenKind::RightParen, TokenKind::Comma, |parser| {
      parser.parse_parameter()
    })?;

    let ty = self.parse_type_annotation(true)?;

    let header = function::Header {
      name,
      type_parameters,
      parameters,
      ty,
    };

    let body = self.parse_expression()?;

    Ok(function::Function { header, body })
  }

  fn parse_struct(&mut self) -> Result<r#struct::Struct<Type>, Error<ParserError>> {
    self.expect(vec![TokenKind::Struct])?;

    let name = self.expect_identifier()?;
    let type_parameters = self.parse_type_parameter()?;
    let traits = self.parse_traits()?;

    let header = r#struct::Header {
      name,
      type_parameters,
      traits,
    };

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

    Ok(r#struct::Struct {
      header,
      module: module::Module { items },
    })
  }

  fn parse_enum(&mut self) -> Result<r#enum::Enum<Type>, Error<ParserError>> {
    self.expect(vec![TokenKind::Enum])?;

    let name = self.expect_identifier()?;
    let type_parameters = self.parse_type_parameter()?;
    let header = r#enum::Header {
      name,
      type_parameters,
    };

    self.expect(vec![TokenKind::LeftBrace])?;

    let variants = self.expect_list(TokenKind::RightBrace, TokenKind::Comma, |parser| {
      parser.parse_enum_variant()
    })?;

    Ok(r#enum::Enum { header, variants })
  }

  fn parse_enum_variant(&mut self) -> Result<r#enum::Variant<Type>, Error<ParserError>> {
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

    Ok(r#enum::Variant { name, fields })
  }

  fn parse_trait(&mut self) -> Result<r#trait::Trait<Type>, Error<ParserError>> {
    self.expect(vec![TokenKind::Trait])?;

    let name = self.expect_identifier()?;
    let type_parameters = self.parse_type_parameter()?;
    let traits = self.parse_traits()?;

    let header = r#trait::Header {
      name,
      type_parameters,
      traits,
    };

    self.expect(vec![TokenKind::LeftBrace])?;

    let mut items = Vec::new();

    loop {
      match self.tokens.peek() {
        Some((_, TokenKind::RightBrace)) => {
          self.tokens.next();
          break;
        }
        _ => items.push((self.parse_modifiers(true), self.parse_trait_item()?)),
      }
    }

    Ok(r#trait::Trait { header, items })
  }

  fn parse_trait_item(&mut self) -> Result<r#trait::Item<Type>, Error<ParserError>> {
    let value = expect! {
      self,
      true,
      TokenKind::Fn = TokenKind::Fn => {
        let name = self.expect_identifier()?;
        let type_parameters = self.parse_type_parameter()?;

        self.expect(vec![TokenKind::LeftParen])?;

        let parameters = self.expect_list(TokenKind::RightParen, TokenKind::Comma, |parser| {
          parser.parse_type()
        })?;

        let ty = self.parse_type_annotation(false)?.unwrap();

        r#trait::Item::Function(r#trait::Function { name, type_parameters, parameters, ty })
      },
      TokenKind::Struct = TokenKind::Struct => {
        let name = self.expect_identifier()?;
        let type_parameters = self.parse_type_parameter()?;
        let traits = self.parse_traits()?;

        r#trait::Item::Struct(r#trait::Struct { name, type_parameters, traits })
      },
      TokenKind::Enum = TokenKind::Enum => {
        let name = self.expect_identifier()?;
        let type_parameters = self.parse_type_parameter()?;

        r#trait::Item::Enum(r#trait::Enum { name, type_parameters })
      },
      TokenKind::Trait = TokenKind::Trait => {
        let name = self.expect_identifier()?;
        let type_parameters = self.parse_type_parameter()?;
        let traits = self.parse_traits()?;

        match self.tokens.peek() {
          Some((_, TokenKind::LeftBrace)) => {
            let header = r#trait::Header { name, type_parameters, traits };

            self.tokens.next();

            let mut items = Vec::new();

            loop {
              match self.tokens.peek() {
                Some((_, TokenKind::RightBrace)) => {
                  self.tokens.next();
                  break;
                }
                _ => items.push((self.parse_modifiers(true), self.parse_trait_item()?)),
              }
            }

            r#trait::Item::Child(r#trait::Trait { header, items })
          }
          _ => r#trait::Item::Trait(r#trait::Trait_ { name, type_parameters, traits })
        }
      },
      TokenKind::Operator(_) = TokenKind::Operator("".to_string()) => {
        let operator = match self.tokens.next() {
          Some((_, TokenKind::Operator(operator))) => operator,
          token => Err(self.unexpected_token(token, vec![TokenKind::Operator("".to_string())]))?,
        }
        .to_string();
        let type_parameters = self.parse_type_parameter()?;

        self.expect(vec![TokenKind::LeftParen])?;

        let a = self.parse_type()?;
        let b = match self.tokens.peek() {
          Some((_, TokenKind::Comma)) => {
            self.tokens.next();
            Some(self.parse_type()?)
          }
          _ => None,
        };

        self.expect(vec![TokenKind::RightParen])?;

        let result = self.parse_type_annotation(false)?.unwrap();
        r#trait::Item::Operator(match b {
          Some(b) => r#trait::Operator::Infix {
            operator,
            type_parameters,
            operands: (a, b),
            result,
          },
          None => r#trait::Operator::Prefix {
            operator,
            type_parameters,
            operand: a,
            result,
          },
        })
      }
    }?;

    self.expect(vec![TokenKind::Semicolon])?;

    Ok(value)
  }

  // TODO: we need to make sure that the types are ultimately concrete
  fn parse_operator(&mut self) -> Result<operator::Operator<Type>, Error<ParserError>> {
    let operator = match self.tokens.next() {
      Some((_, TokenKind::Operator(operator))) => operator,
      token => Err(self.unexpected_token(token, vec![TokenKind::Operator("".to_string())]))?,
    }
    .to_string();
    let type_parameters = self.parse_type_parameter()?;

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

    let result = self.parse_type_annotation(false)?.unwrap();
    let header = match b {
      Some(b) => operator::Header::Infix(operator::Infix {
        operator,
        type_parameters,
        operands: (a, b),
        result,
      }),
      None => operator::Header::Prefix(operator::Prefix {
        operator,
        type_parameters,
        operand: a,
        result,
      }),
    };

    let body = self.parse_expression()?;

    Ok(operator::Operator { header, body })
  }

  fn parse_parameter(&mut self) -> Result<util::Parameter<Type>, Error<ParserError>> {
    let name = self.expect_identifier()?;

    self.expect_operator(vec![":"])?;

    let ty = self.parse_type()?;

    Ok(util::Parameter { name, ty })
  }

  fn parse_expression(&mut self) -> Result<util::Expression<Type>, Error<ParserError>> {
    self.parse_expression_with_precedence(0)
  }

  fn parse_expression_with_precedence(
    &mut self,
    precedence: u8,
  ) -> Result<util::Expression<Type>, Error<ParserError>> {
    let mut expression = match self.tokens.next() {
      Some((_, TokenKind::Operator(operator))) => {
        let operator = operator.to_string();
        let op = TokenKind::Operator(operator.clone());
        Ok(util::Expression::Prefix {
          operator,
          operand: Box::new(self.parse_expression_with_precedence(op.prefix_precedence())?),
        })
      }
      Some((_, TokenKind::Identifier(name))) => Ok(util::Expression::Identifier(name.clone())),
      Some((_, TokenKind::Let)) => Ok(util::Expression::Declaration(self.expect_identifier()?)),
      Some((_, TokenKind::If)) => {
        let condition = Box::new(self.parse_expression()?);
        let consequence = Box::new(self.parse_expression()?);
        let alternative = match self.tokens.peek() {
          Some((_, TokenKind::Else)) => {
            self.tokens.next();
            Some(Box::new(self.parse_expression()?))
          }
          _ => None,
        };

        Ok(util::Expression::If {
          condition,
          consequence,
          alternative,
        })
      }
      Some((_, TokenKind::While)) => {
        let condition = Box::new(self.parse_expression()?);
        let body = Box::new(self.parse_expression()?);

        Ok(util::Expression::While { condition, body })
      }
      Some((_, TokenKind::Return)) => {
        Ok(util::Expression::Return(Box::new(self.parse_expression()?)))
      }
      Some((_, TokenKind::LeftBrace)) => {
        let mut ended = false;
        let mut has_value = false;
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

              has_value = true;
              expressions.push(self.parse_expression()?);

              match self.tokens.peek().copied() {
                Some((_, TokenKind::Semicolon)) => {
                  self.tokens.next();
                  has_value = false;
                }
                _ => {
                  ended = true;
                }
              }
            }
          }
        }

        Ok(util::Expression::Block {
          expressions,
          has_value,
        })
      }
      Some((_, TokenKind::StringLiteral(value))) => Ok(util::Expression::Literal(
        util::Literal::String(value.clone()),
      )),
      Some((_, TokenKind::CharLiteral(value))) => Ok(util::Expression::Literal(
        util::Literal::Char(value.clone()),
      )),
      Some((span, TokenKind::NumberLiteral(value, ty))) => Ok(util::Expression::Literal(
        util::NumberLiteral::from_string(value, ty.clone()).map_err(|e| {
          self.error(
            Some(&(
              span.clone(),
              TokenKind::NumberLiteral(value.clone(), ty.clone()),
            )),
            e,
          )
        })?,
      )),
      Some((_, TokenKind::Fn)) => {
        self.expect(vec![TokenKind::LeftParen])?;

        let parameters = self.expect_list(TokenKind::RightParen, TokenKind::Comma, |parser| {
          parser.parse_parameter()
        })?;

        let ty = self.parse_type_annotation(true)?;
        let body = self.parse_expression()?;

        Ok(util::Expression::Literal(util::Literal::Closure {
          parameters,
          ty,
          body: Box::new(body),
        }))
      }
      Some((_, TokenKind::LeftParen)) => Ok(util::Expression::Literal(util::Literal::Tuple(
        self.expect_list(TokenKind::RightParen, TokenKind::Comma, |parser| {
          parser.parse_expression()
        })?,
      ))),
      Some((_, TokenKind::LeftBracket)) => Ok(util::Expression::Literal(util::Literal::Array(
        self.expect_list(TokenKind::RightBracket, TokenKind::Comma, |parser| {
          parser.parse_expression()
        })?,
      ))),
      Some((_, TokenKind::True)) => Ok(util::Expression::Literal(util::Literal::Bool(true))),
      Some((_, TokenKind::False)) => Ok(util::Expression::Literal(util::Literal::Bool(false))),
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
          expression = util::Expression::Call {
            expression: Box::new(expression),
            arguments: self.expect_list(TokenKind::RightParen, TokenKind::Comma, |parser| {
              parser.parse_expression()
            })?,
          };
        }
        Some((_, TokenKind::LeftBracket)) => {
          self.tokens.next();
          expression = util::Expression::Index {
            expression: Box::new(expression),
            index: Box::new(self.parse_expression()?),
          };
          self.expect(vec![TokenKind::RightBracket])?;
        }
        Some((_, TokenKind::Operator(operator))) => {
          // https://news.ycombinator.com/item?id=13915458
          // TODO: conditional chaining (x == y == z etc.)
          let op = TokenKind::Operator(operator.to_string());
          expression = if op.infix_precedence() >= precedence {
            self.tokens.next();
            util::Expression::Infix {
              operator: operator.to_string(),
              operands: (
                Box::new(expression),
                Box::new(self.parse_expression_with_precedence(
                  op.infix_precedence() + if op.left_associative() { 1 } else { 0 },
                )?),
              ),
            }
          } else {
            break;
          }
        }
        _ => break,
      };
    }

    Ok(expression)
  }

  fn parse_type_parameter(&mut self) -> Result<Vec<util::TypeParameter>, Error<ParserError>> {
    match self.tokens.peek() {
      Some((_, TokenKind::Operator(operator))) if operator == "<" => {
        self.tokens.next();
        let parameters = self.expect_list(
          TokenKind::Operator(">".to_string()),
          TokenKind::Comma,
          |parser| {
            let name = parser.expect_identifier()?;
            Ok(util::TypeParameter {
              name,
              traits: parser.parse_traits()?,
            })
          },
        )?;
        Ok(parameters)
      }
      _ => Ok(Vec::new()),
    }
  }

  fn parse_type(&mut self) -> Result<Type, Error<ParserError>> {
    let expected = vec![
      TokenKind::Identifier("".to_string()),
      TokenKind::Bool,
      TokenKind::Numeric(NumericType::Char),
      TokenKind::Numeric(NumericType::I8),
      TokenKind::Numeric(NumericType::I16),
      TokenKind::Numeric(NumericType::I32),
      TokenKind::Numeric(NumericType::I64),
      TokenKind::Numeric(NumericType::I128),
      TokenKind::Numeric(NumericType::U8),
      TokenKind::Numeric(NumericType::U16),
      TokenKind::Numeric(NumericType::U32),
      TokenKind::Numeric(NumericType::U64),
      TokenKind::Numeric(NumericType::U128),
      TokenKind::Numeric(NumericType::F32),
      TokenKind::Numeric(NumericType::F64),
      TokenKind::LeftParen,
      TokenKind::LeftBracket,
      TokenKind::Fn,
    ];

    let ty = match self.tokens.next() {
      Some(token) => Ok(match &token.1 {
        TokenKind::Identifier(name) => {
          let mut path = vec![name.to_string()];

          match self.tokens.peek() {
            Some((_, TokenKind::Operator(operator))) if operator == "::" => {
              self.tokens.next();
              path.append(&mut self.expect_identifier_list(TokenKind::Operator("::".to_string()))?);
            }
            _ => (),
          }

          Type::Named(
            path,
            match self.tokens.peek() {
              Some((_, TokenKind::Operator(operator))) if operator == "<" => {
                self.tokens.next();
                let parameters = self.expect_list(
                  TokenKind::Operator(">".to_string()),
                  TokenKind::Comma,
                  |parser| parser.parse_type(),
                )?;
                parameters
              }
              _ => Vec::new(),
            },
          )
        }

        TokenKind::Bool => Type::Bool,

        TokenKind::Numeric(NumericType::Char) => Type::Char,
        TokenKind::Numeric(NumericType::I8) => Type::I8,
        TokenKind::Numeric(NumericType::I16) => Type::I16,
        TokenKind::Numeric(NumericType::I32) => Type::I32,
        TokenKind::Numeric(NumericType::I64) => Type::I64,
        TokenKind::Numeric(NumericType::I128) => Type::I128,
        TokenKind::Numeric(NumericType::U8) => Type::U8,
        TokenKind::Numeric(NumericType::U16) => Type::U16,
        TokenKind::Numeric(NumericType::U32) => Type::U32,
        TokenKind::Numeric(NumericType::U64) => Type::U64,
        TokenKind::Numeric(NumericType::U128) => Type::U128,
        TokenKind::Numeric(NumericType::F32) => Type::F32,
        TokenKind::Numeric(NumericType::F64) => Type::F64,
        TokenKind::LeftParen => {
          let list = self.expect_list(TokenKind::RightParen, TokenKind::Comma, |parser| {
            parser.parse_type()
          })?;

          match self.tokens.peek() {
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
    }
    .map(|ty| ty.reduce())?;

    Ok(
      if let Some(&(_, TokenKind::Operator(operator))) = self.tokens.peek() {
        match operator.as_str() {
          "|" => {
            self.tokens.next();

            let mut types = BTreeSet::new();

            match ty {
              Type::Union(mut union) => types.append(&mut union),
              ty => {
                types.insert(ty);
              }
            }

            loop {
              match self.parse_type()? {
                Type::Union(mut union) => types.append(&mut union),
                ty => {
                  types.insert(ty);
                }
              }

              match self.tokens.peek() {
                Some((_, TokenKind::Operator(operator))) if operator == "|" => {
                  self.tokens.next();
                }
                _ => break,
              }
            }

            Type::Union(types)
          }
          _ => ty,
        }
      } else {
        ty
      },
    )
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

  fn parse_traits(&mut self) -> Result<Vec<util::Path>, Error<ParserError>> {
    match self.tokens.peek() {
      Some((_, TokenKind::Operator(operator))) if operator == ":" => {
        self.tokens.next();
        self.expect_list_without_end(TokenKind::Comma, |parser| {
          parser.expect_identifier_list(TokenKind::Operator("::".to_string()))
        })
      }
      _ => Ok(Vec::new()),
    }
  }
}
