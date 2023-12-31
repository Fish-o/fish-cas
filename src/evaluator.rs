use num::{rational::Ratio, traits::Pow, BigInt, BigRational, FromPrimitive};

use crate::{
  parser::{ComparisonType, Expression, FunctionType},
  state::{Function, State, StateError},
};

pub fn evaluate(expressions: &Vec<Expression>) -> Result<Vec<Expression>, ExtendedEvaluationError> {
  let mut state = State::new();

  let mut new_expressions = vec![];
  for (count, expression) in expressions.iter().enumerate() {
    let new_expression = expression.evaluate(&mut state);
    if let Err(err) = new_expression {
      return Err(ExtendedEvaluationError::LocatedEvaluationError(err, count));
    } else {
      new_expressions.push(new_expression.unwrap());
    }
  }
  Ok(new_expressions)
}

impl Expression {
  pub fn evaluate(&self, state: &mut State) -> Result<Expression, EvaluationError> {
    log::debug!(
      "Evaluating: {:#?}, {:#?}",
      match self {
        Expression::Value(_) => "Value",
        Expression::Variable(_) => "Variable",
        Expression::Function(_, _) => "Function",
        Expression::Assignment(_, _) => "Assignment",
        Expression::Boolean(_) => "Boolean",
        Expression::Comparison(_, _, _) => "Comparison",
        Expression::Condition(_, _, _) => "Condition",
      },
      self
    );
    let expression = self;
    Ok(match expression {
      Expression::Value(_) => expression.clone(),
      Expression::Variable(name) => {
        let res = state.recall_variable(name);
        if res.is_some() {
          let exp = res.unwrap();
          if state.is_variable_looping(name, &vec![]) {
            exp
          } else {
            exp.evaluate(state)?
          }
        } else {
          Expression::Variable(name.clone())
        }
      }
      Expression::Function(function_type, arguments) => {
        evaluate_function(state, function_type, arguments)?
      }
      Expression::Assignment(left, right) => match (left.as_ref(), right.as_ref()) {
        (
          Expression::Function(func1_type, func1_args),
          Expression::Function(func2_type, func2_args),
        ) => {
          let (name, arguments, exp)= match (func1_type, func2_type) {
          (FunctionType::Custom(_), FunctionType::Custom(_)) => todo!(),
          (FunctionType::Custom(name), _) => (name, func1_args, right),
          (_,FunctionType::Custom(name)) => (name, func2_args, left),
          _=>panic!("Arbitrary assignment is not supported. Please only assign expressions to custom functions or variables")
          };
          let mut variables = vec![];
          for argument in arguments {
            match argument {
              Expression::Variable(name) => variables.push(name.clone()),
              _ => {
                return Err(EvaluationError::InvalidFunctionArgument(
                  "Only variables can be arguments when assigning a custom function".to_string(),
                ))
              }
            }
          }
          state.store_function(name, variables.clone(), exp.as_ref().clone())?;
          Expression::Boolean(true)
        }

        (Expression::Variable(name), exp) | (exp, Expression::Variable(name)) => {
          state.store_variable(name, exp)?;
          if let Expression::Function(func_type, args) = exp {
            if let FunctionType::Custom(name) = func_type {
              let mut variables = vec![];
              for arg in args {
                match arg {
                  Expression::Variable(ident) => {
                    if variables.contains(ident) {
                      return Err(EvaluationError::DuplicateFunctionArgument(format!(
                        "Argument {} was supplied more than once",
                        ident
                      )));
                    } else {
                      variables.push(ident.clone())
                    }
                  }
                  _ => {
                    return Err(EvaluationError::InvalidFunctionArgument(
                      "Function arguments can only be variables".to_owned(),
                    ))
                  }
                }
              }
              state.store_function(name, variables.clone(), exp.clone())?;
            }
          }
          Expression::Boolean(true)
        }
        (Expression::Function(func_type, args), exp)
        | (exp, Expression::Function(func_type, args)) => match func_type {
          FunctionType::Custom(name) => {
            let mut variables = vec![];
            for arg in args {
              match arg {
                Expression::Variable(ident) => {
                  if variables.contains(ident) {
                    return Err(EvaluationError::DuplicateFunctionArgument(format!(
                      "Argument {} was supplied more than once",
                      ident
                    )));
                  } else {
                    variables.push(ident.clone())
                  }
                }
                _ => {
                  return Err(EvaluationError::InvalidFunctionArgument(
                    "Function arguments can only be variables".to_owned(),
                  ))
                }
              }
            }
            state.store_function(name, variables.clone(), exp.clone())?;
            Expression::Boolean(true)
          }
          _ => {
            return Err(EvaluationError::AssignmentError(
              "Can't assign to operator".to_string(),
            ))
          }
        },
        _ => {
          return Err(EvaluationError::AssignmentError(
            "Can only assign expression to variable or function".to_owned(),
          ));
        }
      },
      Expression::Boolean(_) => expression.clone(),
      Expression::Comparison(comparator, left, right) => {
        let left = left.evaluate(state)?;
        let right = right.evaluate(state)?;
        let left_val = value_from(&left);
        let right_val = value_from(&right);
        let left_bool = bool_from(&left);
        let right_bool = bool_from(&right);
        let val = match (left_val, right_val) {
          (Ok(left), Ok(right)) => {
            let result = match comparator {
              ComparisonType::Equal => left == right,
              ComparisonType::NotEqual => left != right,
              ComparisonType::LessThan => left < right,
              ComparisonType::LessThanOrEqual => left <= right,
              ComparisonType::GreaterThan => left > right,
              ComparisonType::GreaterThanOrEqual => left >= right,
            };
            Some(Expression::Boolean(result))
          }
          _ => None,
        };
        let bool = match (left_bool, right_bool) {
          (Ok(left), Ok(right)) => {
            let result = match comparator {
              ComparisonType::Equal => left == right,
              ComparisonType::NotEqual => left != right,
              ComparisonType::LessThan => left < right,
              ComparisonType::LessThanOrEqual => left <= right,
              ComparisonType::GreaterThan => left > right,
              ComparisonType::GreaterThanOrEqual => left >= right,
            };
            Some(Expression::Boolean(result))
          }
          _ => None,
        };
        match (val, bool) {
          (Some(val), _) => val,
          (_, Some(bool)) => bool,
          _ => Expression::Comparison(comparator.clone(), Box::new(left), Box::new(right)),
        }
      }
      Expression::Condition(if_exp, then_exp, else_exp) => {
        let if_exp = if_exp.evaluate(state)?;
        let bool_res = bool_from(&if_exp);
        if bool_res.is_err() {
          Expression::Condition(Box::new(if_exp), then_exp.clone(), else_exp.clone())
        } else {
          if bool_res.unwrap() {
            then_exp.evaluate(state)?
          } else {
            else_exp.evaluate(state)?
          }
        }
      }
    })
  }
}

fn evaluate_function(
  state: &mut State,
  function_type: &FunctionType,
  arguments: &Vec<Expression>,
) -> Result<Expression, EvaluationError> {
  log::debug!("Evaluating function {:?}", function_type);
  assure_argument_length(state, function_type, arguments)?;
  let evaluated_arguments = arguments
    .iter()
    .map(|el| el.evaluate(state))
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
  let new_exp = match function_type {
    FunctionType::Custom(name) => Some(Function::evaluate(state, name, arguments.clone())?),
    _ => None,
  };
  if all_values && new_exp.is_none() {
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
      FunctionType::Root => {
        let base = value_from(&arguments[0])?;
        let root_base = value_from(&arguments[1])?;
        let root = root_base.numer();
        let power = root_base.denom();

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
      FunctionType::Custom(_) => unreachable!(),
    };
    Ok(Expression::Value(new_value))
  } else if new_exp.is_some() {
    Ok(new_exp.unwrap().evaluate(state)?)
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
fn bool_from(expression: &Expression) -> Result<bool, EvaluationError> {
  match expression {
    Expression::Boolean(val) => Ok(val.clone()),
    _ => Err(EvaluationError::UnknownEvaluationError(
      "Couldn't get boolean but boolean should be there".to_string(),
    )),
  }
}

fn assure_argument_length(
  state: &mut State,
  function_type: &FunctionType,
  arguments: &Vec<Expression>,
) -> Result<(), EvaluationError> {
  let required_length = match function_type {
    FunctionType::Add => 2,
    FunctionType::Subtract => 2,
    FunctionType::Multiply => 2,
    FunctionType::Divide => 2,
    FunctionType::Exponentiation => 2,
    FunctionType::Root => 2,
    FunctionType::Custom(name) => {
      let func = state.recall_function(name);
      if func.is_none() {
        return Err(EvaluationError::FunctionNotFound(name.to_string()));
      }
      func.unwrap().argument_count()
    }
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
  ValueToLarge(String),
  FunctionNotFound(String),
  ArgumentCountMismatch(String),
  DuplicateFunctionArgument(String),
  InvalidFunctionArgument(String),
  StateError(StateError),
  AssignmentError(String),
}

#[derive(Debug, Clone)]
pub enum ExtendedEvaluationError {
  #[allow(dead_code)]
  EvaluationError(EvaluationError),
  LocatedEvaluationError(EvaluationError, usize),
}

impl From<StateError> for EvaluationError {
  fn from(err: StateError) -> Self {
    return Self::StateError(err);
  }
}
