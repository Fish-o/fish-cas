use crate::{algebra::free_variable, evaluator::EvaluationError, parser::Expression};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct State {
  variables: HashMap<String, Expression>,
  functions: HashMap<String, Function>,
}
fn check_equality(exp1: &Expression, exp2: &Expression) -> bool {
  match (exp1, exp2) {
    (Expression::Value(val1), Expression::Value(val2)) => val1 == val2,
    (Expression::Variable(var1), Expression::Variable(var2)) => var1 == var2,
    (Expression::Function(name1, args1), Expression::Function(name2, args2)) => {
      name1 == name2 && args1 == args2
    }
    _ => false,
  }
}
impl State {
  pub fn new() -> Self {
    Self {
      variables: HashMap::new(),
      functions: HashMap::new(),
    }
  }
  pub fn from(vars: HashMap<String, Expression>) -> Self {
    Self {
      variables: vars,
      functions: HashMap::new(),
    }
  }
  pub fn is_variable_looping(&self, variable: &String, been: &Vec<String>) -> bool {
    let new_been = {
      let mut new_been = been.clone();
      new_been.push(variable.clone());
      new_been
    };
    while let Some(e) = self.variables.get(variable) {
      let vars = e.variables_used();
      if vars.is_empty() {
        return false;
      }
      // Remove duplicates
      for var in vars {
        if new_been.contains(&var) {
          return true;
        } else if self.is_variable_looping(&var, &new_been) {
          return true;
        }
      }
    }
    false
  }

  pub fn store_variable(
    &mut self,
    variable: &String,
    exp: &Expression,
  ) -> Result<Expression, StateError> {
    if let Some(stored) = self.variables.get(variable) {
      // stored(x) = exp(x)
      // 0 = exp(x) - stored(x)
      if check_equality(stored, exp) {
        return Ok(stored.clone());
      } else if stored == exp {
        println!("== SOMEHOW WORKED!?");
        return Ok(stored.clone());
      }
      println!("Var {variable}: {} = {}", stored, exp);
      // Collapse all variables
      todo!("Implement a system where you can assign variables multiple times or something, maybe make it compare instead")
    } else {
      if exp.is_algebraic() {
        let used_variables = exp.variables_used();
        for used_var in used_variables {
          let res = free_variable(
            &used_var,
            Expression::Variable(variable.clone()),
            exp.clone(),
          );
          if let Ok(res) = res {
            self.variables.insert(used_var, res);
          } else {
            println!(
              "Error parsing variable: \n{}\n{}\n{}\n{:?}",
              variable, used_var, exp, res
            );
          }
        }
      }
      self.variables.insert(variable.clone(), exp.clone());
    }
    Ok(exp.clone())
  }
  pub fn recall_variable(&self, variable: &String) -> Option<Expression> {
    let data = self.variables.get(variable);
    if let Some(exp) = data {
      return Some(exp.clone());
    }
    None
  }

  pub fn store_function(
    &mut self,
    identifier: &String,
    arguments: Vec<String>,
    expression: Expression,
  ) -> Result<&Function, StateError> {
    if self.functions.contains_key(identifier) {
      Err(StateError::FunctionAlreadyDeclared)
    } else {
      let function = Function {
        arguments,
        expression,
        cached_values: HashMap::new(),
      };
      self.functions.insert(identifier.clone(), function);
      return Ok(
        self
          .recall_function(identifier)
          .expect("Function disappeared!"),
      );
    }
  }

  pub fn recall_function(&mut self, identifier: &String) -> Option<&mut Function> {
    self.functions.get_mut(identifier)
  }

  pub fn clear(&mut self) {
    self.variables.clear();
    self.functions.clear();
  }

  pub fn expand(&mut self, state: State) {
    for (key, value) in state.variables {
      println!("expanding {} = {}", key, value);
      let _ = self.store_variable(&key, &value);
    }
    for (key, value) in state.functions {
      let _ = self.store_function(&key, value.arguments, value.expression);
    }
  }
}

#[derive(Debug, Clone)]
pub enum StateError {
  FunctionAlreadyDeclared,
}

#[derive(Debug, Clone)]
pub struct Function {
  arguments: Vec<String>,
  expression: Expression,
  cached_values: HashMap<Vec<Expression>, Expression>,
}

impl Function {
  pub fn argument_count(&self) -> usize {
    return self.arguments.len();
  }

  pub fn get_expression(
    &self,
    supplied_arguments: &Vec<Expression>,
  ) -> Result<Expression, EvaluationError> {
    if supplied_arguments.len() != self.argument_count() {
      return Err(EvaluationError::ArgumentCountMismatch(format!(
        "Expected {} arguments for function, supplied {}",
        self.argument_count(),
        supplied_arguments.len()
      )));
    }
    let mut new_exp = self.expression.clone();
    for i in 0..self.argument_count() {
      let variable = &self.arguments[i];
      let replacement = &supplied_arguments[i];
      new_exp.substitute_in_place(variable, replacement)
    }
    Ok(new_exp)
  }

  pub fn evaluate(
    state: &mut State,
    name: &String,
    arguments: Vec<Expression>,
  ) -> Result<Expression, EvaluationError> {
    let func = state.recall_function(name);
    if func.is_none() {
      return Err(EvaluationError::FunctionNotFound(name.to_string()));
    }
    let func = func.unwrap();
    let cache = func.cached_values.get(&arguments);
    if let Some(exp) = cache {
      Ok(exp.clone())
    } else {
      let exp = func.get_expression(&arguments)?;
      let exp = exp.evaluate(state)?;
      let func = state.recall_function(name);
      if func.is_none() {
        return Err(EvaluationError::FunctionNotFound(name.to_string()));
      }
      func.unwrap().cached_values.insert(arguments, exp.clone());
      Ok(exp)
    }
  }
}
