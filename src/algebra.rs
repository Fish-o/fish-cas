use std::collections::HashMap;

use num::{
  bigint::{RandBigInt, RandomBits},
  BigInt, BigRational,
};
use rand::Rng;

use crate::{
  parser::{parse, ComparisonType, Expression, FunctionType},
  state::State,
  tokenizer::tokenize,
};

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
      log::error!("Somehow failed, {variable_side} {new_var}");
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
    Expression::Function(_, args) => {
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

#[derive(Debug, Clone)]
pub enum AlgebraError {
  NoVariable,
  SomehowFailed,
}

pub fn expand(expression: &Expression) -> NodeTree {
  let mut tree = NodeTree::new(expression);
  let eqs = generate_equalities();

  let mut prev_size = 0;
  let mut surface = tree.dump_surface();
  while tree.size() != prev_size && surface.len() > 0 {
    log::debug!("Expanding: {}", surface.len());
    prev_size = tree.size();
    for exp in surface {
      for eq in &eqs {
        let new_exps = apply_equality(&exp, eq);
        for new_exp in new_exps {
          log::debug!("{} = {}, using {}={}", exp, new_exp, eq.left(), eq.right());
          let evaluated = new_exp.evaluate(&mut State::new());
          if let Ok(evaluated) = evaluated {
            if evaluated != new_exp {
              tree.add_node(&evaluated, &exp, eq);
            }
          }
          tree.add_node(&new_exp, &exp, eq);
        }
      }
    }
    surface = tree.dump_surface();
  }

  tree
}
#[derive(Debug)]
pub struct NodeTree {
  nodes: HashMap<Expression, Vec<(Expression, Equality)>>,
  surface: Vec<Expression>,
}
impl NodeTree {
  pub fn new(base: &Expression) -> Self {
    let mut tree = Self {
      nodes: HashMap::new(),
      surface: Vec::new(),
    };
    tree.nodes.insert(base.clone(), Vec::new());
    tree.surface.push(base.clone());
    tree
  }
  pub fn get_nodes(&self) -> &HashMap<Expression, Vec<(Expression, Equality)>> {
    &self.nodes
  }
  pub fn add_node(&mut self, expression: &Expression, from: &Expression, using: &Equality) {
    if !self.nodes.contains_key(from) {
      panic!("Invalid reference to node: {:?}", from)
    } else if let Some(nodes) = self.nodes.get_mut(expression) {
      nodes.push((from.clone(), using.clone()));
    } else {
      log::debug!("Adding node: {:?}", expression);
      self.surface.push(expression.clone());
      self
        .nodes
        .insert(expression.clone(), vec![(from.clone(), using.clone())]);
    }
  }

  pub fn size(&self) -> usize {
    self.nodes.len()
  }
  pub fn dump_surface(&mut self) -> Vec<Expression> {
    let mut surface = Vec::new();
    std::mem::swap(&mut surface, &mut self.surface);
    surface
  }
}
#[derive(Debug, Clone)]
pub struct Equality {
  left: Expression,
  right: Expression,
}
fn random_vals(vars: &Vec<String>) -> HashMap<String, Expression> {
  let mut data = HashMap::new();
  for var in vars {
    if !data.contains_key(var) {
      let mut rng = rand::thread_rng();
      let num: BigInt = rng.sample(RandomBits::new(16));
      let mut denom: BigInt = rng.sample(RandomBits::new(8));
      if denom == 0.into() {
        denom = 1.into();
      }
      let rat = BigRational::new(num, denom);
      let exp = Expression::Value(rat);
      data.insert(var.clone(), exp.clone());
    }
  }
  data
}
impl Equality {
  pub fn left(&self) -> &Expression {
    &self.left
  }
  pub fn right(&self) -> &Expression {
    &self.right
  }
  pub fn get_reverse(&self) -> Self {
    Self {
      left: self.right.clone(),
      right: self.left.clone(),
    }
  }
  pub fn variables_used(&self) -> Vec<String> {
    self.left.variables_used()
  }
  fn n(left: Expression, right: Expression) -> Self {
    Self { left, right }
  }
  pub fn quick_check(&self) {
    log::info!("Quick checking: {}={}", self.left, self.right);
    let mut vars: Vec<String> = self.left.variables_used();
    vars.extend(self.right.variables_used());
    for i in 0..100 {
      log::debug!("QuickCheck {i}");
      let data = random_vals(&vars);
      let mut state = State::from(data);
      log::debug!("Evaluating: {:?}", self.left);
      let l_res = self
        .left
        .evaluate(&mut state)
        .expect(&format!("Failed to evaluate left expression"));
      log::debug!("Evaluated left");

      let r_res = self
        .right
        .evaluate(&mut state)
        .expect(&format!("Failed to evaluate right expression"));
      log::debug!("Evaluated right");

      let l_val = match l_res {
        Expression::Value(val) => val,
        _ => panic!("Failed to get left value back when entering random variables"),
      };
      let r_val = match r_res {
        Expression::Value(val) => val,
        _ => panic!("Failed to get right value back when entering random variables"),
      };
      if l_val != r_val {
        panic!("Equality not valid {}={}", self.left, self.right);
      }
    }
  }
}

const EQUALITIES: &'static [&str] = &[
  "a+b=b+a",
  "a*b=b*a",
  "a+(b+c)=(a+b)+c",
  "a*(b*c)=(a*b)*c",
  "a*(b+c)=a*b+a*c",
  "a*(b/c)=(a*b)/c",
  "a/(b*c)=(a/b)/c",
  "a/(b+c)=(a/b)/(c/b)",
  "(a*b)/b=a",
];

pub fn check_if_matching(
  expression: &Expression,
  pattern: &Expression,
) -> Option<HashMap<String, Expression>> {
  // let mut results = Vec::new();
  // Try to match the left arm of the equality
  let mut state = HashMap::new();
  match (pattern, expression) {
    (Expression::Value(p_val), Expression::Value(val)) => {
      if p_val != val {
        // PATTERN DOESN'T MATCH
      }
    }
    (Expression::Function(f, p_args), Expression::Function(f2, args2)) => {
      if f != f2 {
        // PATTERN DOESN'T MATCH
        return None;
      } else if p_args.len() != args2.len() {
        // PATTERN DOESN'T MATCH MAYBE?
      } else {
        for i in 0..p_args.len() {
          let p_arg = &p_args[i];
          let arg = &args2[i];
          let new_state = check_if_matching(arg, p_arg);
          if let Some(new_state) = new_state {
            for entry in new_state {
              if state.contains_key(&entry.0) {
                if state.get(&entry.0) != Some(&entry.1) {
                  log::error!(
                    "Inconsistent state! {:?} {:?}",
                    state.get(&entry.0).unwrap(),
                    entry.1
                  );
                  return None;
                }
              } else {
                state.insert(entry.0, entry.1);
              }
            }
          } else {
            return None;
          }
        }
      }
    }
    (Expression::Variable(var), exp) => {
      // Check if variable is already set
      if let Some(val) = state.get(var) {
        if val != exp {
          return None;
        }
      } else {
        state.insert(var.clone(), exp.clone());
      }
    }
    _ => {
      return None;
    }
  }

  return Some(state);
}

pub fn apply_equality(expression: &Expression, equality: &Equality) -> Vec<Expression> {
  let mut results = Vec::new();
  // Try to match the left arm of the equality
  let left = equality.left();
  let right = equality.right();
  let left_match = check_if_matching(expression, left);
  if let Some(left_match) = left_match {
    // If it matches, substitute the right arm of the equality
    let mut new_expression = right.clone();
    let variables_used = equality.variables_used();
    for var in variables_used {
      let expr: Option<&Expression> = left_match.get(&var);
      if let Some(expr) = expr {
        new_expression.substitute_in_place(&var, expr);
      } else {
        panic!("Failed to find variable: {}", var);
      }
    }
    results.push(new_expression);
  }
  match expression {
    Expression::Value(_) => {}
    Expression::Variable(_) => {}
    Expression::Function(func_type, args) => {
      for (index, arg) in args.iter().enumerate() {
        let equalities = apply_equality(arg, equality);
        for eq in equalities {
          let mut new_args = args.clone();
          new_args[index] = eq;
          results.push(Expression::Function(func_type.clone(), new_args));
        }
      }
    }
    Expression::Assignment(_, _) => {}
    Expression::Boolean(_) => {}
    Expression::Comparison(_, _, _) => {}
    Expression::Condition(_, _, _) => {}
  }
  results
}

pub fn generate_equalities() -> Vec<Equality> {
  let expressions = EQUALITIES
    .iter()
    .flat_map(|e| {
      parse(tokenize(e).expect(&format!("Could not tokenize expression: {e}")))
        .expect(&format!("Could not parse expression: {e}"))
    })
    .collect::<Vec<_>>();
  let equalities = expressions
    .iter()
    .map(|e| match e {
      Expression::Comparison(ComparisonType::Equal, left, right) => {
        Equality::n(*left.clone(), *right.clone())
      }
      Expression::Assignment(left, right) => Equality::n(*left.clone(), *right.clone()),
      _ => panic!("Not an equality (=): {:?}", e),
    })
    .collect::<Vec<_>>();
  // equalities.iter().for_each(|eq| eq.quick_check());
  log::info!("All equalities passed quick check!");
  equalities
}
