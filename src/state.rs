use crate::{evaluator::EvaluationError, parser::Expression};
use num::BigRational;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct State {
  variables: HashMap<String, Expression>,
  functions: HashMap<String, Function>,
}

impl State {
  pub fn new() -> Self {
    Self {
      variables: HashMap::new(),
      functions: HashMap::new(),
    }
  }
  pub fn store_variable(
    &mut self,
    variable: &String,
    exp: &Expression,
  ) -> Result<Expression, StateError> {
    if let Some(val) = self.variables.get(variable) {
      todo!("Implement a system where you can assign variables multiple times or something, maybe make it compare instead")
    } else {
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
        arguments: arguments,
        expression: expression,
      };
      self.functions.insert(identifier.clone(), function);
      return Ok(
        self
          .recall_function(identifier)
          .expect("Function disappeared!"),
      );
    }
  }

  pub fn recall_function(&self, identifier: &String) -> Option<&Function> {
    self.functions.get(identifier)
  }
}

#[derive(Debug, Clone)]
pub enum StateError {
  ContradictoryStateError(String),
  FunctionAlreadyDeclared,
  UnknownStateError(String),
}
#[derive(Debug, Clone)]
pub enum VariableState {
  Numeric(BigRational),
  // Relation(String),
  // Function(Vec<String>, Expression),
}

#[derive(Debug, Clone)]
pub struct Function {
  arguments: Vec<String>,
  expression: Expression,
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
}
