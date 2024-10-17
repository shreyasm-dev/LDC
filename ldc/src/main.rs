use lexer::lexer::Lexer;
use parser::parser::Parser;
use std::process;

mod error;
mod lexer;
mod parser;

fn main() {
  let source = r#"
  pub enum X {
    Y,
    Z(char, [(i32, xyz): (abc, char)]),
    A(b, c | d | e | f)
  }

  fn main(): () {
    (x)().y(if a b else if c d else e).z;
    while x y;
    x = y = z;
    [if a b] = c
  }

  op -(a: b): c {
    d
  }

  op +(a: b, c: d): e f[g]

  // very readable
  fn x(): fn(a, b): c fn(y: a, z: b): c a + b * c + d > e -> f
"#
  .trim();

  let mut lexer = Lexer::new(source);
  match lexer.lex(false) {
    Ok(tokens) => {
      for token in tokens.clone() {
        println!("{:?}", token);
      }

      let mut parser = Parser::new(tokens.iter().peekable());
      let ast = parser.parse();

      match ast {
        Ok(ast) => println!("{:#?}", ast),
        Err(error) => {
          error.print(source, "[]");
          process::exit(1);
        }
      }
    }
    Err(error) => {
      error.print(source, "[]");
      process::exit(1);
    }
  }
}
