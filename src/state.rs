use crate::parser::Expression;
use num::BigRational;
use std::{collections::HashMap, process::id};

#[derive(Debug, Clone)]
pub struct State {
  variables: HashMap<String, BigRational>,
  functions: HashMap<String, Function>,
}

impl State {
  pub fn new() -> Self {
    Self {
      variables: HashMap::new(),
      functions: HashMap::new(),
    }
  }
  pub fn store_numeric_variable(
    &mut self,
    variable: &String,
    number: &BigRational,
  ) -> Result<BigRational, StateError> {
    if let Some(val) = self.variables.get(variable) {
      if val != number {
        return Err(StateError::ContradictoryStateError(format!(
          "Can't set variable '{}' to '{}', because it was previously set to '{}'",
          variable, number, val
        )));
      }
    } else {
      self.variables.insert(variable.clone(), number.clone());
    }
    Ok(number.clone())
  }
  pub fn recall_numeric_variable(&self, variable: &String) -> Option<BigRational> {
    let data = self.variables.get(variable);
    if let Some(numb) = data {
      return Some(numb.clone());
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
