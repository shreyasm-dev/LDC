use super::scope::{Item, ItemKind, Scope};
use crate::{error::TypecheckerError, parser::ast, union};
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use uuid::Uuid;

type Type = ast::util::Type<String>;
type Tagged = ast::util::Type<Uuid>;

#[derive(Debug, Clone)]
pub struct Typechecker {
  pub types: HashMap<Uuid, Item>,
}

impl Typechecker {
  pub fn new() -> Typechecker {
    Typechecker {
      types: HashMap::new(),
    }
  }

  pub fn typecheck(
    &mut self,
    module: ast::module::Module<Type>,
  ) -> Result<(), TypecheckerError<Type>> {
    self.typecheck_module(None, module).map(|_| ())
  }

  pub fn typecheck_module(
    &mut self,
    parent: Option<Rc<RefCell<Scope>>>,
    module: ast::module::Module<Type>,
  ) -> Result<(), TypecheckerError<Type>> {
    let mut scope = Scope::new(parent);

    for item in &module.items {
      match &item.kind {
        ast::module::ItemKind::Function(f) => {
          scope.insert(
            f.header.name.clone(),
            Item::new(ItemKind::Function(f.clone())),
          );
        }
        ast::module::ItemKind::Struct(s) => {
          let item = Item::new(ItemKind::Struct(s.clone()));
          scope.insert(s.header.name.clone(), item.clone());
          self.types.insert(item.0, item);
        }
        _ => todo!(),
      }
    }

    let scope_ref = Rc::new(RefCell::new(scope));
    for item in &module.items {
      match &item.kind {
        ast::module::ItemKind::Function(f) => {
          self.typecheck_function(scope_ref.clone(), f.clone())?;
        }
        ast::module::ItemKind::Struct(s) => {}
        _ => todo!(),
      }
    }

    Ok(())
  }

  pub fn typecheck_function(
    &self,
    parent: Rc<RefCell<Scope>>,
    function: ast::function::Function<Type>,
  ) -> Result<(), TypecheckerError<Type>> {
    let mut scope = Scope::new(Some(parent));

    for parameter in &function.header.parameters {
      scope.insert(
        parameter.name.clone(),
        Item::new(ItemKind::Variable(parameter.ty.clone())),
      );
    }

    let body = self.typecheck_expression(Rc::new(RefCell::new(scope)), function.body.clone())?;

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
    &self,
    parent: Rc<RefCell<Scope>>,
    expression: ast::util::Expression<Type>,
  ) -> Result<Type, TypecheckerError<Type>> {
    match expression {
      ast::util::Expression::Block {
        expressions,
        has_value,
      } => {
        let mut value = None;

        for expression in expressions {
          value = Some(self.typecheck_expression(parent.clone(), expression)?);
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
        let expression_type = self.typecheck_expression(parent.clone(), *expression)?;

        match expression_type {
          Type::Function(parameters, r#type) => {
            if parameters.len() != arguments.len() {
              Err(TypecheckerError::InvalidArguments {
                expected: parameters.clone(),
                found: arguments
                  .iter()
                  .map(|a| self.typecheck_expression(parent.clone(), a.clone()))
                  .collect::<Result<_, _>>()?,
              })?
            }

            for (parameter, argument) in parameters.iter().zip(arguments.clone()) {
              let argument = self.typecheck_expression(parent.clone(), argument)?;

              if !argument.satisfies(parameter) {
                Err(TypecheckerError::InvalidArguments {
                  expected: parameters.clone(),
                  found: arguments
                    .iter()
                    .map(|a| self.typecheck_expression(parent.clone(), a.clone()))
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
      ast::util::Expression::Identifier(name) => match parent.borrow().get(&name) {
        Some(Item(_, ItemKind::Variable(ty))) => Ok(ty.clone()),
        Some(Item(_, ItemKind::Function(f))) => Ok(Type::Function(
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
        let condition = self.typecheck_expression(parent.clone(), *condition)?;

        if !condition.satisfies(&Type::Bool) {
          Err(TypecheckerError::InvalidType {
            expected: Type::Bool,
            found: condition,
          })?
        }

        let consequence: Type = self.typecheck_expression(parent.clone(), *consequence)?;

        if let Some(alternative) = alternative {
          Ok(union!(
            consequence,
            self.typecheck_expression(parent, *alternative)?
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
            .map(|e| self.typecheck_expression(parent.clone(), e))
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
          let mut scope = Scope::new(Some(parent));
          for parameter in &parameters {
            scope.insert(
              parameter.name.clone(),
              Item::new(ItemKind::Variable(parameter.ty.clone())),
            );
          }

          let body = self.typecheck_expression(Rc::new(RefCell::new(scope)), *body)?;

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
        self.typecheck_expression(parent.clone(), *expression)
      }
      ast::util::Expression::While { condition, body } => {
        let condition = self.typecheck_expression(parent.clone(), *condition)?;

        if !condition.satisfies(&Type::Bool) {
          Err(TypecheckerError::InvalidType {
            expected: Type::Bool,
            found: condition,
          })?
        }

        Ok(Type::Array(Box::new(
          self.typecheck_expression(parent.clone(), *body)?,
        )))
      }
      _ => todo!(),
    }
  }
}
