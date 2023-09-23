use std::collections::HashMap;

use num::{rational::Ratio, traits::Pow, BigInt, BigRational, FromPrimitive};

use crate::{
  parser::{Expression, FunctionType},
  state::State,
};

pub fn evaluate(expressions: &Vec<Expression>) -> Result<Vec<Expression>, ExtendedEvaluationError> {
  let mut state = State::new();

  let mut new_expressions = vec![];
  for (count, expression) in expressions.iter().enumerate() {
    let new_expression = evaluate_single(expression);
    if let Err(err) = new_expression {
      return Err(ExtendedEvaluationError::LocatedEvaluationError(err, count));
    } else {
      new_expressions.push(new_expression.unwrap());
    }
  }
  Ok(new_expressions)
}

fn evaluate_single(expression: &Expression) -> Result<Expression, EvaluationError> {
  Ok(match expression {
    Expression::Value(_) => expression.clone(),
    Expression::Variable(_) => expression.clone(),
    Expression::Function(function_type, arguments) => evaluate_function(function_type, arguments)?,
    Expression::Assignment(_, _) => todo!(),
  })
}

fn evaluate_function(
  function_type: &FunctionType,
  arguments: &Vec<Expression>,
) -> Result<Expression, EvaluationError> {
  assure_argument_length(function_type, arguments)?;
  let evaluated_arguments = arguments
    .iter()
    .map(|el| evaluate_single(el))
    .collect::<Vec<_>>();
  let mut all_values = true;
  let mut arguments = vec![];
  for argument in evaluated_arguments {
    if let Ok(exp) = argument {
      if match exp {
        Expression::Value(_) => false,
        _ => true,
      } {
        all_values = false;
      }
      arguments.push(exp);
    } else {
      return Err(argument.unwrap_err());
    }
  }
  if all_values {
    let new_value = match function_type {
      FunctionType::Add => value_from(&arguments[0])? + value_from(&arguments[1])?,
      FunctionType::Subtract => value_from(&arguments[0])? - value_from(&arguments[1])?,
      FunctionType::Multiply => value_from(&arguments[0])? * value_from(&arguments[1])?,
      FunctionType::Divide => value_from(&arguments[0])? / value_from(&arguments[1])?,
      FunctionType::Exponentiation => {
        let base = value_from(&arguments[0])?;
        let exponent = value_from(&arguments[1])?;
        let power = exponent.numer();
        let root = exponent.denom();

        let result = base.pow(power);

        if root == &BigInt::from_u32(1).expect("Could't create number 1") {
          result
        } else {
          // nthRoot(a/b, n) = nthRoot(a, n) / nthRoot(b, n)
          let numer = result.numer();
          let denom = result.denom();
          let root = root.try_into();
          if let Err(_) = root {
            return Err(EvaluationError::ValueToLarge(
              "Trying to take nth root with n being to large".to_string(),
            ));
          }
          let root: u32 = root.unwrap();
          let numer = numer.nth_root(root);
          let denom = denom.nth_root(root);
          Ratio::new(numer, denom)
        }
      }
      FunctionType::Custom(_) => todo!(),
    };
    Ok(Expression::Value(new_value))
  } else {
    Ok(Expression::Function(function_type.clone(), arguments))
  }
}
fn value_from(expression: &Expression) -> Result<BigRational, EvaluationError> {
  match expression {
    Expression::Value(val) => Ok(val.clone()),
    _ => Err(EvaluationError::UnknownEvaluationError(
      "Couldn't get value but value should be there".to_string(),
    )),
  }
}
fn assure_argument_length(
  function_type: &FunctionType,
  arguments: &Vec<Expression>,
) -> Result<(), EvaluationError> {
  let required_length = match function_type {
    FunctionType::Add => 2,
    FunctionType::Subtract => 2,
    FunctionType::Multiply => 2,
    FunctionType::Divide => 2,
    FunctionType::Exponentiation => 2,
    FunctionType::Custom(_) => todo!(),
  };
  let actual_length = arguments.len();
  if actual_length != required_length {
    return Err(EvaluationError::ArgumentCountMismatch(format!(
      "Expected {} arguments for operator {}, found {}",
      required_length, function_type, actual_length
    )));
  }

  Ok(())
}
#[derive(Debug, Clone)]
pub enum EvaluationError {
  UnknownEvaluationError(String),
  ArgumentCountMismatch(String),
  ValueToLarge(String),
}

#[derive(Debug, Clone)]
pub enum ExtendedEvaluationError {
  EvaluationError(EvaluationError),
  LocatedEvaluationError(EvaluationError, usize),
}
