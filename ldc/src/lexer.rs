use crate::{
  error::LexerError,
  token::{Token, TokenKind},
  util::Span,
};
use std::{iter::Peekable, str::Chars};

pub struct Lexer<'a> {
  pub input: Peekable<Chars<'a>>,
  pub start: usize,
  pub end: usize,
}

impl<'a> Lexer<'a> {
  pub fn new(input: &'a str) -> Lexer<'a> {
    Lexer {
      input: input.chars().peekable(),
      start: 0,
      end: 0,
    }
  }

  pub fn advance(&mut self) -> Option<char> {
    self.end += 1;
    self.input.next()
  }

  pub fn reset(&mut self) {
    self.start = self.end;
  }

  pub fn span(&self) -> Span {
    (self.start, self.end)
  }

  pub fn next(&mut self) -> Result<Token, LexerError> {
    let result = match self.advance() {
      Some(c) => match c {
        ' ' | '\n' | '\r' | '\t' => {
          let mut whitespace = String::new();
          whitespace.push(c);

          while let Some(&c) = self.input.peek() {
            match c {
              ' ' | '\n' | '\r' | '\t' => {
                whitespace.push(c);
                self.advance();
              }
              _ => break,
            }
          }

          Ok(TokenKind::Whitespace(whitespace))
        }

        'a'..='z' | 'A'..='Z' | '_' => {
          let mut ident = String::new();
          ident.push(c);

          while let Some(&c) = self.input.peek() {
            match c {
              'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                ident.push(c);
                self.advance();
              }
              _ => break,
            }
          }

          Ok(match ident.as_str() {
            "fn" => TokenKind::Fn,
            "struct" => TokenKind::Struct,
            "enum" => TokenKind::Enum,
            "trait" => TokenKind::Trait,
            "let" => TokenKind::Let,

            "bool" => TokenKind::Bool,

            _ => TokenKind::Identifier(ident),
          })
        }

        '(' => Ok(TokenKind::LeftParen),
        ')' => Ok(TokenKind::RightParen),
        '{' => Ok(TokenKind::LeftBrace),
        '}' => Ok(TokenKind::RightBrace),
        '[' => Ok(TokenKind::LeftBracket),
        ']' => Ok(TokenKind::RightBracket),
        '<' => Ok(TokenKind::LeftAngle),
        '>' => Ok(TokenKind::RightAngle),

        ',' => Ok(TokenKind::Comma),
        ':' => Ok(TokenKind::Colon),
        ';' => Ok(TokenKind::Semicolon),
        '.' => Ok(TokenKind::Dot),

        _ => Err(LexerError::UnexpectedCharacter(self.span(), c)),
      },
      None => Ok(TokenKind::Eof),
    }?;

    let span = self.span();
    self.reset();

    Ok((span, result))
  }

  pub fn lex(&mut self, emit_whitespace: bool) -> Result<Vec<Token>, LexerError> {
    let mut tokens = Vec::new();

    loop {
      let (span, kind) = self.next()?;

      if kind == TokenKind::Eof {
        break;
      }

      if let TokenKind::Whitespace(_) = kind {
        if !emit_whitespace {
          continue;
        }
      }

      tokens.push((span, kind));
    }

    Ok(tokens)
  }
}
