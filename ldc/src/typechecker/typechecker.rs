use super::scope::{Item, Scope};
use crate::{error::TypecheckerError, parser::ast, union};

type Type = ast::util::Type;

#[derive(Debug, Clone)]
pub struct Typechecker;

impl Typechecker {
  pub fn typecheck(module: ast::module::Module<Type>) -> Result<Scope, TypecheckerError<Type>> {
    let mut scope = Scope::new(None);

    for item in &module.items {
      match &item.kind {
        ast::module::ItemKind::Function(f) => {
          scope.insert(f.header.name.clone(), Item::Function(f.clone()));
        }
        _ => todo!(),
      }
    }

    for item in &module.items {
      match &item.kind {
        ast::module::ItemKind::Function(f) => {
          Self::typecheck_function(scope.clone(), f.clone())?;
        }
        _ => todo!(),
      }
    }

    Ok(scope)
  }

  pub fn typecheck_function(
    parent: Scope,
    function: ast::function::Function<Type>,
  ) -> Result<(), TypecheckerError<Type>> {
    let mut scope = parent.clone();

    for parameter in &function.header.parameters {
      scope.insert(parameter.name.clone(), Item::Variable(parameter.ty.clone()));
    }

    let body = Self::typecheck_expression(scope, function.body.clone())?;

    if let Some(ty) = &function.header.ty {
      if !body.satisfies(ty) {
        Err(TypecheckerError::InvalidType {
          expected: ty.clone(),
          found: body,
        })?
      }
    }

    Ok(())
  }

  pub fn typecheck_expression(
    parent: Scope,
    expression: ast::util::Expression<Type>,
  ) -> Result<Type, TypecheckerError<Type>> {
    match expression {
      ast::util::Expression::Block {
        expressions,
        has_value,
      } => {
        let mut value = None;

        for expression in expressions {
          value = Some(Self::typecheck_expression(parent.clone(), expression)?);
        }

        if has_value {
          Ok(value.unwrap_or(Type::Tuple(vec![])))
        } else {
          Ok(Type::Tuple(vec![]))
        }
      }
      ast::util::Expression::Call {
        expression,
        arguments,
      } => {
        let expression_type = Self::typecheck_expression(parent.clone(), *expression)?;

        match expression_type {
          Type::Function(parameters, r#type) => {
            if parameters.len() != arguments.len() {
              Err(TypecheckerError::InvalidArguments {
                expected: parameters.clone(),
                found: arguments
                  .iter()
                  .map(|a| Self::typecheck_expression(parent.clone(), a.clone()))
                  .collect::<Result<_, _>>()?,
              })?
            }

            for (parameter, argument) in parameters.iter().zip(arguments.clone()) {
              let argument = Self::typecheck_expression(parent.clone(), argument)?;

              if !argument.satisfies(parameter) {
                Err(TypecheckerError::InvalidArguments {
                  expected: parameters.clone(),
                  found: arguments
                    .iter()
                    .map(|a| Self::typecheck_expression(parent.clone(), a.clone()))
                    .collect::<Result<_, _>>()?,
                })?
              }
            }

            Ok(*r#type)
          }
          _ => Err(TypecheckerError::Todo),
        }
      }
      // TODO: functions, etc.
      ast::util::Expression::Identifier(name) => match parent.get(&name) {
        Some(Item::Variable(ty)) => Ok(ty.clone()),
        Some(Item::Function(f)) => Ok(Type::Function(
          f.header.parameters.iter().map(|p| p.ty.clone()).collect(),
          Box::new(f.header.ty.clone().unwrap_or(Type::Tuple(vec![]))),
        )),
        _ => Err(TypecheckerError::Todo),
      },
      ast::util::Expression::If {
        condition,
        consequence,
        alternative,
      } => {
        let condition = Self::typecheck_expression(parent.clone(), *condition)?;

        if !condition.satisfies(&Type::Bool) {
          Err(TypecheckerError::InvalidType {
            expected: Type::Bool,
            found: condition,
          })?
        }

        let consequence: Type = Self::typecheck_expression(parent.clone(), *consequence)?;

        if let Some(alternative) = alternative {
          Ok(union!(
            consequence,
            Self::typecheck_expression(parent, *alternative)?
          ))
        } else {
          Ok(consequence)
        }
      }
      ast::util::Expression::Literal(literal) => match literal {
        ast::util::Literal::Char(_) => Ok(Type::Char),
        ast::util::Literal::String(_) => todo!(),
        ast::util::Literal::Tuple(vec) => Ok(Type::Tuple(
          vec
            .into_iter()
            .map(|e| Self::typecheck_expression(parent.clone(), e))
            .collect::<Result<_, _>>()?,
        )),
        ast::util::Literal::Number(n) => Ok(n.into()),
        ast::util::Literal::Array(vec) => {
          // TODO: inference
          if vec.is_empty() {
            Err(TypecheckerError::Todo)?
          }

          todo!()
        }
        ast::util::Literal::Bool(_) => Ok(Type::Bool),
        ast::util::Literal::Closure {
          parameters,
          ty,
          body,
        } => {
          let mut scope = parent.clone();
          for parameter in &parameters {
            scope.insert(parameter.name.clone(), Item::Variable(parameter.ty.clone()));
          }

          let body = Self::typecheck_expression(scope, *body)?;

          if let Some(ty) = ty {
            if !body.satisfies(&ty) {
              Err(TypecheckerError::InvalidType {
                expected: ty.clone(),
                found: body.clone(),
              })?
            }
          }

          Ok(Type::Function(
            parameters.iter().map(|p| p.ty.clone()).collect(),
            Box::new(body),
          ))
        }
      },
      // TODO: make sure the types of return statements match with the type of blocks
      ast::util::Expression::Return(expression) => {
        Self::typecheck_expression(parent.clone(), *expression)
      }
      ast::util::Expression::While { condition, body } => {
        let condition = Self::typecheck_expression(parent.clone(), *condition)?;

        if !condition.satisfies(&Type::Bool) {
          Err(TypecheckerError::InvalidType {
            expected: Type::Bool,
            found: condition,
          })?
        }

        Ok(Type::Array(Box::new(Self::typecheck_expression(
          parent.clone(),
          *body,
        )?)))
      }
      _ => todo!(),
    }
  }
}
