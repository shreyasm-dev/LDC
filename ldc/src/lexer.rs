use crate::{
  error::{Error, LexerError},
  token::{Token, TokenKind},
};
use std::{
  iter::Peekable,
  ops::{Range, RangeInclusive},
  str::Chars,
};

macro_rules! escape {
  ($l:lifetime, $span:expr, $x:expr, $string:expr, $($c:expr => $r:expr),* $(,)?) => {
    match $x {
      $(
        $c => $string.push($r),
      )*
      _ => {
        break $l Err(Error(
          $span,
          LexerError::UnexpectedCharacter($x, &[], &[$($c),*]),
        ));
      },
    }
  };
}

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

  pub fn span(&self) -> Range<usize> {
    self.start..self.end
  }

  pub fn unexpected_character(
    &self,
    c: char,
    ranges: &'static [RangeInclusive<char>],
    characters: &'static [char],
  ) -> Error<LexerError> {
    Error(
      self.last_char(),
      LexerError::UnexpectedCharacter(c, ranges, characters),
    )
  }

  pub fn eof(&self) -> Error<LexerError> {
    Error((self.end - 1)..(self.end - 1), LexerError::UnexpectedEof)
  }

  pub fn last_char(&self) -> Range<usize> {
    (self.end - 1)..self.end
  }

  pub fn next(&mut self) -> Result<Token, Error<LexerError>> {
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

        '"' => 'string: {
          let mut string = String::new();

          loop {
            match self.advance() {
              Some('"') => break,
              Some(c) => match c {
                '\\' => match self.advance() {
                  Some(c) => {
                    escape!(
                      'string,
                      self.last_char(),
                      c,
                      string,
                      'n' => '\n',
                      'r' => '\r',
                      't' => '\t',
                      '0' => '\0',
                      '\\' => '\\',
                      '"' => '"',
                      // TODO: unicode escape
                      '{' => {
                        let mut code = String::new();

                        loop {
                          match self.advance() {
                            Some(c) => match c {
                              '0'..='9' | 'a'..='f' | 'A'..='F' => code.push(c),
                              '}' => break,
                              _ => {
                                break 'string Err(self.unexpected_character(
                                  c,
                                  &['0'..='9', 'a'..='f', 'A'..='F'],
                                  &['}'],
                                ))
                              }
                            },
                            None => {
                              break 'string Err(self.eof())
                            }
                          }
                        }

                        match u32::from_str_radix(&code, 16) {
                          Ok(codepoint) => {
                            match std::char::from_u32(codepoint) {
                              Some(c) => c,
                              None => {
                                break 'string Err(Error(
                                  self.last_char(),
                                  LexerError::InvalidCodepoint(code),
                                ))
                              }
                            }
                          }
                          Err(_) => {
                            break 'string Err(Error(
                              self.last_char(),
                              LexerError::InvalidCodepoint(code),
                            ))
                          }
                        }
                      }
                    )
                  }
                  None => break 'string Err(self.eof()),
                },
                c => string.push(c),
              },
              None => break 'string Err(self.eof()),
            }
          }

          Ok(TokenKind::StringLiteral(string))
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

        _ => Err(self.unexpected_character(c, &[], &[])),
      },
      None => Ok(TokenKind::Eof),
    }?;

    let span = self.span();
    self.reset();

    Ok((span, result))
  }

  pub fn lex(&mut self, emit_whitespace: bool) -> Result<Vec<Token>, Error<LexerError>> {
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

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_general() {
    let source = r#"fn main(args: [string]) {
  "hello, world!\n";
}"#;

    let mut lexer = Lexer::new(source);
    let tokens = lexer.lex(false).unwrap();

    assert_eq!(
      tokens,
      vec![
        ((0..2), TokenKind::Fn),
        ((3..7), TokenKind::Identifier("main".to_string())),
        ((7..8), TokenKind::LeftParen),
        ((8..12), TokenKind::Identifier("args".to_string())),
        ((12..13), TokenKind::Colon),
        ((14..15), TokenKind::LeftBracket),
        ((15..21), TokenKind::Identifier("string".to_string())),
        ((21..22), TokenKind::RightBracket),
        ((22..23), TokenKind::RightParen),
        ((24..25), TokenKind::LeftBrace),
        (
          (28..45),
          TokenKind::StringLiteral("hello, world!\n".to_string())
        ),
        ((45..46), TokenKind::Semicolon),
        ((47..48), TokenKind::RightBrace),
      ]
    );
  }

  #[test]
  fn test_string() {
    assert_eq!(
      Lexer::new(r#""hello, world!\n""#).lex(false).unwrap(),
      vec![(
        (0..17),
        TokenKind::StringLiteral("hello, world!\n".to_string())
      )]
    );

    assert_eq!(
      Lexer::new(r#""hello, world!\n"#).lex(false).unwrap_err(),
      Error(16..16, LexerError::UnexpectedEof)
    );

    assert_eq!(
      Lexer::new(r#""hello, world!\x""#).lex(false).unwrap_err(),
      Error(
        15..16,
        LexerError::UnexpectedCharacter('x', &[], &['n', 'r', 't', '0', '\\', '"', '{'])
      )
    );

    assert_eq!(
      Lexer::new(r#""hello, world!\{""#).lex(false).unwrap_err(),
      Error(
        16..17,
        LexerError::UnexpectedCharacter('"', &['0'..='9', 'a'..='f', 'A'..='F'], &['}'])
      )
    );

    assert_eq!(
      Lexer::new(r#""hello, world!\{"#).lex(false).unwrap_err(),
      Error(16..16, LexerError::UnexpectedEof)
    );
  }
}
