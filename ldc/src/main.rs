use lexer::Lexer;

mod error;
mod lexer;
mod token;
mod util;

fn main() {
  let source = r#"
  fn main(args: [string]) {}
  "#;

  let mut lexer = Lexer::new(source);
  match lexer.lex(true) {
    Ok(tokens) => {
      for token in tokens {
        println!("{:?}", token);
      }
    }
    Err(e) => {
      eprintln!("{}", e);
    }
  }
}
