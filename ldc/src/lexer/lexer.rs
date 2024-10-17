use super::token::{Token, TokenKind};
use crate::error::{Error, LexerError};
use std::{
  iter::Peekable,
  ops::{Range, RangeInclusive},
  str::Chars,
};

macro_rules! escape {
  ($span:expr, $x:expr, $string:expr, $($c:expr => $r:expr),* $(,)?) => {
    match $x {
      $(
        $c => Ok($r),
      )*
      _ => {
        Err(Error(
          $span,
          LexerError::UnexpectedCharacter($x, &[], &[$($c),*]),
        ))
      },
    }
  }
}

macro_rules! match_operators {
  ($self:ident, $c:expr, $else:expr => $($op:expr)* $(,)?) => {
    $(
      if $c == $op.chars().next().unwrap() && 'matches: {
        let mut cloned = $self.clone();

        for (_, expected) in $op.chars().enumerate().skip(1) {
          match cloned.input.peek() {
            Some(&c) if c == expected => {
              cloned.advance();
            }
            _ => {
              break 'matches false;
            }
          }
        }

        let _ = std::mem::replace($self, cloned);

        true
      } {
        Ok(TokenKind::Operator($op.to_string()))
      }
    ) else* else {
      $else
    }
  };
}

#[derive(Debug, Clone)]
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
    Error(
      (self.end - 1)..(self.end - 1),
      LexerError::UnexpectedEof(self.start),
    )
  }

  pub fn last_char(&self) -> Range<usize> {
    (self.end - 1)..self.end
  }

  pub fn match_until<F>(&mut self, c: char, f: F) -> String
  where
    F: Fn(char) -> Option<String>,
  {
    let mut result = String::new();
    result.push(c);

    while let Some(&c) = self.input.peek() {
      match f(c) {
        Some(s) => {
          result.push_str(&s);
          self.advance();
        }
        None => break,
      }
    }

    result
  }

  pub fn next(&mut self) -> Result<Token, Error<LexerError>> {
    let result = match self.advance() {
      Some(c) => match c {
        ' ' | '\n' | '\r' | '\t' => Ok(TokenKind::Whitespace(self.match_until(c, |c| match c {
          ' ' | '\n' | '\r' | '\t' => Some(c.to_string()),
          _ => None,
        }))),

        'a'..='z' | 'A'..='Z' | '_' => Ok(TokenKind::from_identifier(self.match_until(
          c,
          |c| match c {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => Some(c.to_string()),
            _ => None,
          },
        ))),

        '"' => {
          let mut string = String::new();

          loop {
            string.push(match self.lex_char()? {
              '"' => break,
              c => c,
            });
          }

          Ok(TokenKind::StringLiteral(string))
        }

        '\'' => {
          let c = self.lex_char()?;
          match self.advance() {
            Some('\'') => Ok(TokenKind::CharLiteral(c)),
            Some(c) => Err(self.unexpected_character(c, &[], &['\'']))?,
            None => Err(self.eof())?,
          }
        }

        '(' => Ok(TokenKind::LeftParen),
        ')' => Ok(TokenKind::RightParen),
        '{' => Ok(TokenKind::LeftBrace),
        '}' => Ok(TokenKind::RightBrace),
        '[' => Ok(TokenKind::LeftBracket),
        ']' => Ok(TokenKind::RightBracket),

        ',' => Ok(TokenKind::Comma),
        ';' => Ok(TokenKind::Semicolon),

        '/' => match self.input.peek() {
          Some('/') => {
            self.advance();

            let mut comment = String::new();

            while let Some(&c) = self.input.peek() {
              match c {
                '\n' => break,
                c => {
                  comment.push(c);
                  self.advance();
                }
              }
            }

            Ok(TokenKind::Comment(comment))
          }
          Some('*') => {
            self.advance();

            let mut comment = String::new();

            loop {
              match self.advance() {
                Some('*') => match self.advance() {
                  Some('/') => break,
                  Some(c) => {
                    comment.push('*');
                    comment.push(c);
                  }
                  None => Err(self.eof())?,
                },
                Some(c) => comment.push(c),
                None => Err(self.eof())?,
              }
            }

            Ok(TokenKind::Comment(comment))
          }
          _ => Ok(TokenKind::Operator("/".to_string())),
        },

        _ => {
          match_operators!(
            self,
            c,
            Err(self.unexpected_character(c, &[], &[])) =>

            ">>>"
            "..="
            "..<"

            "=="
            "!="
            "<="
            ">="
            "&&"
            "||"
            ">>"
            "<<"
            "->"
            ".."
            "??"

            "<"
            ">"
            ":"
            "."
            "="
            "+"
            "-"
            "*"
            "%"
            "!"
            "&"
            "|"
            "^"
            "?"
            "~"
          )
        }
      },
      None => Ok(TokenKind::Eof),
    }?;

    let span = self.span();
    self.reset();

    Ok((span, result))
  }

  pub fn lex_char(&mut self) -> Result<char, Error<LexerError>> {
    match self.advance() {
      Some(c) => match c {
        '\\' => match self.advance() {
          Some(c) => {
            escape!(
              self.last_char(),
              c,
              string,
              'n' => '\n',
              'r' => '\r',
              't' => '\t',
              '0' => '\0',
              '\\' => '\\',
              '"' => '"',
              '{' => {
                let mut code = String::new();

                loop {
                  match self.advance() {
                    Some(c) => match c {
                      '0'..='9' | 'a'..='f' | 'A'..='F' => code.push(c),
                      '}' => break,
                      _ => {
                        Err(self.unexpected_character(
                          c,
                          &['0'..='9', 'a'..='f', 'A'..='F'],
                          &['}'],
                        ))?
                      }
                    },
                    None => {
                      Err(self.eof())?
                    }
                  }
                }

                match u32::from_str_radix(&code, 16) {
                  Ok(codepoint) => {
                    match std::char::from_u32(codepoint) {
                      Some(c) => c,
                      None => {
                        Err(Error(
                          self.last_char(),
                          LexerError::InvalidCodepoint(code),
                        ))?
                      }
                    }
                  }
                  Err(_) => {
                    Err(Error(
                      self.last_char(),
                      LexerError::InvalidCodepoint(code),
                    ))?
                  }
                }
              }
            )
          }
          None => Err(self.eof()),
        },
        c => Ok(c),
      },
      None => Err(self.eof()),
    }
  }

  pub fn lex(&mut self, emit_ignored: bool) -> Result<Vec<Token>, Error<LexerError>> {
    let mut tokens = Vec::new();

    loop {
      let (span, kind) = self.next()?;

      if kind == TokenKind::Eof {
        tokens.push((span.start..span.start, kind));
        break;
      }

      if let TokenKind::Whitespace(_) | TokenKind::Comment(_) = kind {
        if !emit_ignored {
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
    assert_eq!(
      Lexer::new(
        r#"fn main(args: [string]) {
  "hello, world!\n";
}"#
      )
      .lex(false)
      .unwrap(),
      vec![
        ((0..2), TokenKind::Fn),
        ((3..7), TokenKind::Identifier("main".to_string())),
        ((7..8), TokenKind::LeftParen),
        ((8..12), TokenKind::Identifier("args".to_string())),
        ((12..13), TokenKind::Operator(":".to_string())),
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
        ((48..48), TokenKind::Eof)
      ]
    );
  }

  #[test]
  fn test_string() {
    assert_eq!(
      Lexer::new(r#""hello, world!\n""#).lex(false).unwrap(),
      vec![
        (
          (0..17),
          TokenKind::StringLiteral("hello, world!\n".to_string())
        ),
        ((17..17), TokenKind::Eof)
      ]
    );

    assert_eq!(
      Lexer::new(r#""hello, world!\n"#).lex(false).unwrap_err(),
      Error(16..16, LexerError::UnexpectedEof(0))
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
      Error(16..16, LexerError::UnexpectedEof(0))
    );
  }

  #[test]
  fn test_comment() {
    assert_eq!(
      Lexer::new(r#"// hello, world!"#).lex(true).unwrap(),
      vec![
        (0..16, TokenKind::Comment(" hello, world!".to_string())),
        (16..16, TokenKind::Eof)
      ]
    );

    assert_eq!(
      Lexer::new(r#"/* hello, world! */"#).lex(true).unwrap(),
      vec![
        (0..19, TokenKind::Comment(" hello, world! ".to_string())),
        (19..19, TokenKind::Eof)
      ]
    );

    assert_eq!(
      Lexer::new(r#"/* hello, world!"#).lex(true).unwrap_err(),
      Error(16..16, LexerError::UnexpectedEof(0))
    );

    assert_eq!(
      Lexer::new(
        r#"/* hello, world!
\\**\/*/"#
      )
      .lex(true)
      .unwrap(),
      vec![
        (
          0..25,
          TokenKind::Comment(
            r#" hello, world!
\\**\/"#
              .to_string()
          )
        ),
        (25..25, TokenKind::Eof)
      ]
    );
  }
}
