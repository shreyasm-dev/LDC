#[macro_export]
macro_rules! union {
  ($a:expr, $($b:expr),*) => {
    {
      let mut set = std::collections::BTreeSet::new();
      set.insert($a);
      $(set.insert($b);)*
      crate::parser::ast::util::Type::Union(set)
    }
  };
}

#[macro_export]
macro_rules! map0 {
  ($x:expr, $from:path, $to:path, $post:expr, $($p:ident),*) => {
    match $x {
      $(
        <$from>::$p => $post(<$to>::$p),
      )*
      _ => unreachable!(),
    }
  }
}

#[macro_export]
macro_rules! map1 {
  ($x:expr, $from:ident, $to:ident, $post:expr, $($p:ident),*) => {
    match $x {
      $(
        $from::$p(_) => $post($to::$p),
      )*
      _ => unreachable!(),
    }
  }
}

#[macro_export]
macro_rules! expect {
  ($self:ident, $next:expr, $($token:pat = $value:expr => $block:block),*) => {
    match if $next { $self.tokens.next() } else { $self.tokens.peek().copied() } {
      $(Some((_, $token)) => Ok($block),)*
      token => Err($self.unexpected_token(token, vec![$($value),*])),
    }
  }
}

#[macro_export]
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

#[macro_export]
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
