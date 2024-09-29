use lexer::lexer::Lexer;
use std::process;

mod error;
mod lexer;
mod util;

fn main() {
  let source = r#"
fn main(args: [string]) {
  // comment
  /* comment
  \**\/*/
  "xyz\nabc
\{48}\{45}\{4c}\{4c}\{4f}"
}
"#
  .trim();

  let mut lexer = Lexer::new(source);
  match lexer.lex(true) {
    Ok(tokens) => {
      for token in tokens {
        println!("{:?}", token);
      }
    }
    Err(error) => {
      error.print(source, "[]");
      process::exit(1);
    }
  }
}
