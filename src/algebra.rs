use crate::parser::{Expression, FunctionType};

pub fn free_variable(
  variable: &String,
  left: Expression,
  right: Expression,
) -> Result<Expression, AlgebraError> {
  // Find the variable in the left expression
  let left_has_variable = has_variable(variable, &left);
  let right_has_variable = has_variable(variable, &right);
  let mut variable_side = left;
  let mut constant_side = right;

  if !left_has_variable && !right_has_variable {
    return Err(AlgebraError::NoVariable);
  } else if right_has_variable {
    (variable_side, constant_side) = (constant_side, variable_side);
  }

  loop {
    let mut done = false;
    let new_var = match &variable_side {
      Expression::Value(_) => unreachable!(),
      Expression::Boolean(_) => unreachable!(),
      Expression::Variable(var) => {
        assert!(var == variable);
        done = true;
        Expression::Variable(variable.clone())
      }
      Expression::Function(func_type, args) => {
        let (new_variable, new_constant) = match func_type {
          FunctionType::Add => {
            let left = &args[0];
            let right = &args[1];
            let (var_exp, constant) = if has_variable(variable, left) {
              (left, right)
            } else {
              (right, left)
            };
            let constant = Expression::Function(
              FunctionType::Subtract,
              vec![constant_side, constant.clone()],
            );
            (var_exp, constant)
          }
          FunctionType::Subtract => {
            let left = &args[0];
            let right = &args[1];
            let (var_exp, constant) = if has_variable(variable, left) {
              (left, right)
            } else {
              (right, left)
            };
            let constant =
              Expression::Function(FunctionType::Add, vec![constant_side, constant.clone()]);
            (var_exp, constant)
          }
          FunctionType::Multiply => {
            let left = &args[0];
            let right = &args[1];
            let (var_exp, constant) = if has_variable(variable, left) {
              (left, right)
            } else {
              (right, left)
            };
            let constant =
              Expression::Function(FunctionType::Divide, vec![constant_side, constant.clone()]);
            (var_exp, constant)
          }
          FunctionType::Divide => {
            let left = &args[0];
            let right = &args[1];
            let (var_exp, constant) = if has_variable(variable, left) {
              (left, right)
            } else {
              (right, left)
            };
            let constant = Expression::Function(
              FunctionType::Multiply,
              vec![constant_side, constant.clone()],
            );
            (var_exp, constant)
          }
          FunctionType::Exponentiation => {
            let base = &args[0];
            let exponent = &args[1];
            if has_variable(variable, base) {
              let constant =
                Expression::Function(FunctionType::Root, vec![constant_side, exponent.clone()]);
              (base, constant)
            } else {
              todo!("Implement logarithms :)")
              // let constant =
              //   Expression::Function(FunctionType::Exponentiation, vec![base.clone(), exponent]);
              // (exponent, constant)
            }
          }
          FunctionType::Root => {
            let base = &args[0];
            let root = &args[1];
            if has_variable(variable, base) {
              let constant = Expression::Function(
                FunctionType::Exponentiation,
                vec![constant_side, root.clone()],
              );
              (base, constant)
            } else {
              todo!("Implement logarithms :)")
              // let constant =
              //   Expression::Function(FunctionType::Exponentiation, vec![base.clone(), exponent]);
              // (exponent, constant)
            }
          }
          FunctionType::Custom(_) => todo!(),
        };
        constant_side = new_constant;
        new_variable.clone()
      }
      Expression::Assignment(_, _) => unimplemented!(),
      Expression::Comparison(_, _, _) => unimplemented!(),
      Expression::Condition(_, _, _) => unimplemented!(),
    };
    if done {
      break;
    } else if variable_side == new_var {
      println!("Somehow failed, {variable_side} {new_var}");
      return Err(AlgebraError::SomehowFailed);
    } else {
      variable_side = new_var;
    }
  }
  Ok(constant_side)
}

pub fn has_variable(variable: &String, expression: &Expression) -> bool {
  match expression {
    Expression::Value(_) => false,
    Expression::Variable(var) => var == variable,
    Expression::Assignment(_, _) => false,
    Expression::Boolean(_) => false,
    Expression::Function(name, args) => {
      for arg in args {
        if has_variable(variable, arg) {
          return true;
        }
      }
      false
    }
    Expression::Comparison(_, _, _) => false,
    Expression::Condition(_, _, _) => false,
  }
}

pub fn get_inverse(func: FunctionType) -> FunctionType {
  match func {
    FunctionType::Add => FunctionType::Subtract,
    FunctionType::Subtract => FunctionType::Add,
    FunctionType::Multiply => FunctionType::Divide,
    FunctionType::Divide => FunctionType::Multiply,
    FunctionType::Exponentiation => FunctionType::Root,
    FunctionType::Root => FunctionType::Exponentiation,
    FunctionType::Custom(_) => panic!("Cannot get inverse of custom function"),
  }
}

#[derive(Debug, Clone)]
pub enum AlgebraError {
  NoVariable,
  SomehowFailed,
}
