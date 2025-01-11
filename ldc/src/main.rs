use lexer::lexer::Lexer;
use parser::parser::Parser;
use std::process;
use typechecker::typechecker::Typechecker;

mod error;
mod lexer;
mod parser;
mod typechecker;
mod util;

// TODO: allow top-level variables
// TODO: fn x() {}; should just be syntactic sugar for let x = fn() {};
fn main() {
  let name = "[]";
  /* let source = r#"
    pub enum X {
      Y,
      Z(char, [(i32, xyz): (abc, char)]),
      A(b, c | d | e | f)
    };

    fn main(): () {
      (x)().y(if a b else if c d else e).z;
      while x y;
      let x = y = z;
      [if a b] = c
    };

    - <T>(a: B<T>): C<i32> {
      d
    };

    + (a: b, c: d): e f['\{41}'];

    // very readable
    fn x(): fn(a, b): c fn(y: a, z: b): c !(a + b * c + d > e -> ~~f + g);

    trait T {
      pub fn x(a, b): c;
      static fn y(c): ();

      struct Z: Y;

      trait U {
        pub fn z<Y: T>(): c;
      };
    };

    struct X: T {
      pub fn x() y;
      pub static fn y() x;

      pub struct Z: T::U {
        pub fn z<U: T>() 65c.0c;
      };
    };
  "#
    .trim(); */
  let source = r#"
pub fn hello(a: a::b, b: c, c: c, x: bool): a::b | (a::b, c) {
  if x a else hello(a, c, b, x)
};

pub struct Util {
  pub fn x(a: i32, b: i32): i32 {
    choose(a, b, true)
  };

  pub fn y(a: i32): i32 x(a, a);

  pub static fn negate(x: bool): bool {
    if x false else true
  };

  pub static fn choose(a: i32, b: i32, x: bool): i32 {
    if negate(x) a else b
  };

  pub struct Helpers {
    pub fn y() x(1, 2);
  };
};
"#
  .trim();

  let mut lexer = Lexer::new(source);
  match lexer.lex(false) {
    Ok(tokens) => {
      for _ in tokens.clone() {
        // println!("{:?}", token);
      }

      let mut parser = Parser::new(tokens.iter().peekable());
      let ast: Result<
        parser::ast::module::Module<parser::ast::util::Type<Vec<String>>>,
        error::Error<error::ParserError>,
      > = parser.parse();

      match ast {
        Ok(ast) => {
          // println!("{:#?}", ast);

          let mut typechecker = Typechecker::new();
          let result = typechecker.typecheck(ast.clone());

          match result {
            Ok(_) => {
              println!("Typechecked successfully")
            }
            Err(error) => match error {
              error::TypecheckerError::InvalidType { expected, found } => {
                println!("Type {} does not satisfy expected type {}", found, expected)
              }
              error::TypecheckerError::InvalidArguments { expected, found } => println!(
                "Invalid arguments: expected {}, found {}",
                expected
                  .iter()
                  .map(ToString::to_string)
                  .collect::<Vec<String>>()
                  .join(", "),
                found
                  .iter()
                  .map(ToString::to_string)
                  .collect::<Vec<String>>()
                  .join(", ")
              ),
              error::TypecheckerError::UnresolvedIdentifier(name) => {
                println!("Unresolved identifier {}", name)
              }
            },
          }
        }
        Err(error) => {
          error.print(source, name);
          process::exit(1);
        }
      }
    }
    Err(error) => {
      error.print(source, name);
      process::exit(1);
    }
  }
}
