use num::{BigRational, FromPrimitive};

use crate::tokenizer::{Comparator, Keyword, Operator, Token};
pub fn parse(token_stream: Vec<Token>) -> Result<Vec<Expression>, ExtendedParserError> {
  let lines = token_stream
    .split(|el| match el {
      Token::EndOfExpression => true,
      _ => false,
    })
    .collect::<Vec<_>>();

  let mut expressions = vec![];
  for (index, token_stream) in lines.iter().enumerate() {
    let mut token_stream = token_stream.to_vec();
    if token_stream.len() == 0 {
      continue;
    }
    let indices = token_stream
      .iter()
      .enumerate()
      .filter(|(_, t)| match t {
        Token::Assignment => true,
        _ => false,
      })
      .map(|(i, _)| i)
      .collect::<Vec<_>>();
    if indices.len() > 1 {
      return Err(ExtendedParserError::LocatedParserError(
        ParserError::InvalidAssignment("More then 1 assignment present in expression".to_string()),
        index,
      ));
    } else if indices.len() == 1 {
      let assignment_index = indices[0];
      let left = token_stream.drain(0..assignment_index).collect::<Vec<_>>();
      let right = token_stream
        .drain(1..token_stream.len())
        .collect::<Vec<_>>();
      let left = parse(left);
      let right = parse(right);
      if let Err(l_err) = left {
        match l_err {
          ExtendedParserError::ParserError(e) | ExtendedParserError::LocatedParserError(e, _) => {
            return Err(ExtendedParserError::LocatedParserError(e, index))
          }
        }
      } else if let Err(r_err) = right {
        match r_err {
          ExtendedParserError::ParserError(e) | ExtendedParserError::LocatedParserError(e, _) => {
            return Err(ExtendedParserError::LocatedParserError(e, index))
          }
        }
      }
      let left = left
        .unwrap()
        .first()
        .expect("Left hand side no first element")
        .clone();
      let right = right
        .unwrap()
        .first()
        .expect("Right hand side no first element")
        .clone();
      let expression = Expression::Assignment(Box::new(left), Box::new(right));
      expressions.push(expression);
    } else {
      let batched_tokens = batch(&token_stream);
      if let Err(err) = batched_tokens {
        return Err(ExtendedParserError::LocatedParserError(err, index));
      }
      let expression = parse_batches(batched_tokens.unwrap());
      if let Err(err) = expression {
        return Err(ExtendedParserError::LocatedParserError(err, index));
      }
      expressions.push(expression.unwrap())
    }
  }
  Ok(expressions)
}

pub fn batch(tokens: &Vec<Token>) -> Result<Vec<Token>, ParserError> {
  let mut bracket_count = 0;
  let mut index = 0;

  let length = tokens.len();
  let mut new_tokens = vec![];
  let mut new_batch = vec![];
  while index < length {
    let token = &tokens[index];

    match token {
      Token::BracketOpen => {
        if bracket_count > 0 {
          new_batch.push(token.clone())
        }
        bracket_count += 1;
      }
      Token::BracketClose => {
        if bracket_count == 0 {
          return Err(ParserError::BatchingError(
            "To many closing brackets".to_string(),
          ));
        }
        bracket_count -= 1;
        if bracket_count > 0 {
          new_batch.push(token.clone())
        } else {
          new_tokens.push(Token::Batch(Box::new(batch(&new_batch)?)));
          new_batch.clear();
        }
      }
      _ => {
        if bracket_count > 0 {
          new_batch.push(token.clone());
        } else {
          new_tokens.push(token.clone());
        }
      }
    };
    index += 1;
  }
  if bracket_count > 0 {
    return Err(ParserError::BatchingError(
      "Expected closing bracket, but didn't find any".to_string(),
    ));
  }
  Ok(new_tokens)
}

pub fn parse_batches(tokens: Vec<Token>) -> Result<Expression, ParserError> {
  if tokens.len() == 1 {
    return Ok(parse_token(&tokens[0])?);
  }

  // Parse Negate and create Functions
  let mut tokens = tokens;
  let mut index = 0;
  let mut length = tokens.len();
  while index + 1 < length {
    let token = &tokens[index];
    let next_token = &tokens[index + 1];
    match (token, next_token) {
      (Token::Negate, Token::Negate) => {
        tokens.remove(index);
        tokens.remove(index);
        length -= 2;
      }
      (Token::Identifier(ident), Token::Batch(batch)) => {
        let arguments_tokens = batch
          .split(|el| match el {
            Token::Separator => true,
            _ => false,
          })
          .collect::<Vec<_>>();

        let mut arguments = vec![];
        for batch in arguments_tokens {
          arguments.push(parse_batches(batch.to_vec())?);
        }
        let expr = Expression::Function(FunctionType::Custom(ident.to_string()), arguments);
        let _ = std::mem::replace(&mut tokens[index], Token::Expression(expr));
        tokens.remove(index + 1);
        length -= 1;
      }
      (Token::Negate, Token::Batch(batch)) => {
        let batch = parse_batches(batch.to_vec())?;
        let expr = Expression::Function(
          FunctionType::Multiply,
          vec![
            Expression::Value(BigRational::from_i32(-1).expect("Couldn't create number '-1'")),
            batch,
          ],
        );
        let _ = std::mem::replace(&mut tokens[index], Token::Expression(expr));
        tokens.remove(index + 1);
        length -= 1;
      }
      (Token::Negate, Token::Identifier(ident)) => {
        let mut should_negate = true;
        let identifier = ident.clone();
        if index + 2 < length {
          let next_el = &tokens[index + 2];
          match next_el {
            Token::Batch(batch) => {
              should_negate = false;
              let arguments_tokens = batch
                .split(|el| match el {
                  Token::Separator => true,
                  _ => false,
                })
                .collect::<Vec<_>>();

              let mut arguments = vec![];
              for batch in arguments_tokens {
                arguments.push(parse_batches(batch.to_vec())?);
              }
              let function_expr =
                Expression::Function(FunctionType::Custom(ident.clone()), arguments);
              let multiply_expr = Expression::Function(
                FunctionType::Multiply,
                vec![
                  Expression::Value(
                    BigRational::from_i32(-1).expect("Couldn't create number '-1'"),
                  ),
                  function_expr,
                ],
              );
              let _ = std::mem::replace(&mut tokens[index], Token::Expression(multiply_expr));
              tokens.remove(index + 1);
              length -= 1;
            }
            _ => {}
          }
        }

        if should_negate {
          let variable = Expression::Variable(identifier);
          let expr = Expression::Function(
            FunctionType::Multiply,
            vec![
              Expression::Value(BigRational::from_i32(-1).expect("Couldn't create number '-1'")),
              variable,
            ],
          );
          let _ = std::mem::replace(&mut tokens[index], Token::Expression(expr));
          tokens.remove(index + 1);
          length -= 1;
        }
      }
      (Token::Identifier(ident), Token::Number(numb))
      | (Token::Number(numb), Token::Identifier(ident)) => {
        let variable = Expression::Variable(ident.clone());
        let expr = Expression::Function(
          FunctionType::Multiply,
          vec![variable, Expression::Value(numb.clone())],
        );
        let _ = std::mem::replace(&mut tokens[index], Token::Expression(expr));
        tokens.remove(index + 1);
        length -= 1;
      }

      _ => {}
    }
    index += 1;
  }

  // Do all operations in correct order
  let mut order = 0;
  while order < 3 {
    let mut index = 0;
    let length = tokens.len();
    while index < length {
      let token = &tokens[index];
      if let Token::Operator(operator) = token {
        if get_order(operator) == order {
          if index < 1 {
            return Err(ParserError::LeftHandMissing(
              "Left hand of operator not found".to_string(),
            ));
          } else if index == length - 1 {
            return Err(ParserError::RightHandMissing(
              "Right hand of operator not found".to_string(),
            ));
          }
          let left = &tokens[index - 1];
          let right = &tokens[index + 1];
          let left = parse_token(left)?;
          let right = parse_token(right)?;
          let function_type = match operator {
            Operator::Add => FunctionType::Add,
            Operator::Subtract => FunctionType::Subtract,
            Operator::Multiply => FunctionType::Multiply,
            Operator::Divide => FunctionType::Divide,
            Operator::Power => FunctionType::Exponentiation,
          };
          let expression = Expression::Function(function_type, vec![left, right]);
          let _ = std::mem::replace(&mut tokens[index], Token::Expression(expression));
          tokens.remove(index + 1);
          tokens.remove(index - 1);
          index = 0;
          break;
        }
      }
      index += 1;
    }
    if index == length {
      order += 1;
    }
  }

  // Turn all numbers and booleans into expressions
  for index in 0..tokens.len() {
    let token = &tokens[index];
    let new_token = match token {
      Token::Boolean(bool) => Some(Token::Expression(Expression::Boolean(bool.clone()))),
      Token::Number(number) => Some(Token::Expression(Expression::Value(number.clone()))),
      _ => None,
    };
    if let Some(token) = new_token {
      let _ = std::mem::replace(&mut tokens[index], token);
    }
  }

  // Parse comparisons
  let mut index = 1;
  let mut length = tokens.len();
  while index + 1 < length {
    let prev_token = &tokens[index - 1];
    let token = &tokens[index];
    let next_token = &tokens[index + 1];
    match token {
      Token::Comparator(comparator) => {
        let left = parse_token(prev_token)?;
        let right = parse_token(next_token)?;
        let comparator = match comparator {
          Comparator::Equal => ComparisonType::Equal,
          Comparator::NotEqual => ComparisonType::NotEqual,
          Comparator::LessThan => ComparisonType::LessThan,
          Comparator::GreaterThan => ComparisonType::GreaterThan,
          Comparator::LessThanOrEqual => ComparisonType::LessThanOrEqual,
          Comparator::GreaterThanOrEqual => ComparisonType::GreaterThanOrEqual,
        };
        let expression =
          Expression::Comparison(comparator.clone(), Box::new(left), Box::new(right));
        let _ = std::mem::replace(&mut tokens[index], Token::Expression(expression));
        tokens.remove(index + 1);
        tokens.remove(index - 1);
        length -= 2;
        index = 0;
      }
      _ => {}
    }
    index += 1;
  }

  // Parse conditions
  let mut index = 0;
  let mut length = tokens.len();
  while index + 5 < length {
    let if_token = &tokens[index];
    let if_exp_token = &tokens[index + 1];
    let then_token = &tokens[index + 2];
    let then_exp_token = &tokens[index + 3];
    let else_token = &tokens[index + 4];
    let else_exp_token = &tokens[index + 5];
    match (
      if_token,
      if_exp_token,
      then_token,
      then_exp_token,
      else_token,
      else_exp_token,
    ) {
      (
        Token::Keyword(if_kw),
        Token::Expression(if_exp),
        Token::Keyword(then_kw),
        Token::Expression(then_exp),
        Token::Keyword(else_kw),
        Token::Expression(else_exp),
      ) => {
        if if_kw.clone() == Keyword::If
          && then_kw.clone() == Keyword::Then
          && else_kw.clone() == Keyword::Else
        {
          let expression = Expression::Condition(
            Box::new(if_exp.clone()),
            Box::new(then_exp.clone()),
            Box::new(else_exp.clone()),
          );
          let _ = std::mem::replace(&mut tokens[index], Token::Expression(expression));

          tokens.remove(index + 5);
          tokens.remove(index + 4);
          tokens.remove(index + 3);
          tokens.remove(index + 2);
          tokens.remove(index + 1);

          length -= 5;
          index = 0;
        }
      }
      _ => {}
    }
    index += 1;
  }

  if tokens.iter().any(|t| match t {
    Token::Keyword(_) => true,
    _ => false,
  }) {
    println!("{:#?}", tokens);
    return Err(ParserError::UnexpectedToken(
      "Not all keywords parsed successfully".to_string(),
    ));
  }

  if tokens.len() > 1 {
    println!("{:?}", tokens);
    return Err(ParserError::UnknownParserError(
      "To many tokens left".to_string(),
    ));
  }
  let token = tokens.first().expect("Last token disappeared?!");
  if let Token::Expression(exp) = token {
    return Ok(exp.clone());
  } else {
    println!("{:?}", tokens);
    return Err(ParserError::UnknownParserError(
      "Last token not an expression".to_string(),
    ));
  }
}
fn parse_token(token: &Token) -> Result<Expression, ParserError> {
  Ok(match token {
    Token::Number(number) => Expression::Value(number.clone()),
    Token::Boolean(bool) => Expression::Boolean(bool.clone()),
    Token::Identifier(identifier) => Expression::Variable(identifier.clone()),
    Token::Operator(operator) => {
      return Err(ParserError::UnexpectedToken(format!(
        "Unexpected operator '{}'",
        operator
      )))
    }
    Token::Comparator(comparator) => {
      return Err(ParserError::UnexpectedToken(format!(
        "Unexpected comparator '{}'",
        comparator
      )))
    }
    Token::Batch(tokens) => parse_batches(tokens.to_vec())?,
    Token::Expression(expression) => expression.clone(),
    Token::Keyword(keyword) => {
      return Err(ParserError::UnexpectedToken(format!(
        "Unexpected keyword '{}'",
        keyword
      )))
    }
    Token::BracketClose
    | Token::BracketOpen
    | Token::Assignment
    | Token::EndOfExpression
    | Token::Negate
    | Token::Separator => {
      unreachable!()
    }
  })
}
fn get_order(op: &Operator) -> u32 {
  match op {
    Operator::Power => 0,
    Operator::Multiply => 1,
    Operator::Divide => 1,
    Operator::Add => 2,
    Operator::Subtract => 2,
  }
}

#[derive(Debug, Clone)]
pub enum ExtendedParserError {
  #[allow(dead_code)]
  ParserError(ParserError),
  LocatedParserError(ParserError, usize),
}
#[derive(Debug, Clone)]
pub enum ParserError {
  BatchingError(String),
  LeftHandMissing(String),
  RightHandMissing(String),
  UnexpectedToken(String),
  UnknownParserError(String),
  InvalidAssignment(String),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expression {
  Value(BigRational),
  Boolean(bool),
  Variable(String),
  Function(FunctionType, Vec<Expression>),
  Assignment(Box<Expression>, Box<Expression>),
  Comparison(ComparisonType, Box<Expression>, Box<Expression>),
  Condition(Box<Expression>, Box<Expression>, Box<Expression>),
}

impl std::fmt::Display for Expression {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let str = match self {
      Expression::Value(val) => val.to_string(),
      Expression::Boolean(bool) => bool.to_string(),
      Expression::Variable(ident) => ident.clone(),
      Expression::Function(func_type, args) => match func_type {
        FunctionType::Add => format!("({}+{})", &args[0], &args[1]),
        FunctionType::Subtract => format!("({}-{})", &args[0], &args[1]),
        FunctionType::Multiply => format!("({}*{})", &args[0], &args[1]),
        FunctionType::Divide => format!("({}/{})", &args[0], &args[1]),
        FunctionType::Exponentiation => format!("({}^{})", &args[0], &args[1]),
        FunctionType::Root => format!("nthRoot({},{})", &args[0], &args[1]),
        // FunctionType::Logarithm => format!("logn({},{})", &args[0], &args[1]),
        FunctionType::Custom(name) => {
          let args_count = args.len();
          let args = args.iter().map(|arg| format!("{}", arg));
          let mut args_str = String::new();
          for (index, arg) in args.enumerate() {
            args_str += &arg;
            if index + 1 < args_count {
              args_str += ", ";
            }
          }
          format!("{name}({args_str})")
        }
      },
      Expression::Assignment(l, r) => format!("{}={}", l.as_ref(), r.as_ref()),
      Expression::Comparison(comp_type, l, r) => {
        format!("{}{}{}", l.as_ref(), comp_type, r.as_ref())
      }
      Expression::Condition(if_exp, then_exp, else_exp) => format!(
        "if {} then {} else {}",
        if_exp.as_ref(),
        then_exp.as_ref(),
        else_exp.as_ref()
      ),
    };
    write!(f, "{}", str)
  }
}
impl Expression {
  pub fn substitute_in_place(&mut self, variable: &str, expression: &Expression) {
    match self {
      Expression::Boolean(_) => {}
      Expression::Value(_) => {}
      Expression::Variable(ident) => {
        if ident == variable {
          let _ = std::mem::replace(self, expression.clone());
        }
      }
      Expression::Function(_, args) => {
        for arg in args {
          arg.substitute_in_place(variable, expression)
        }
      }
      Expression::Assignment(exp1, exp2) => {
        exp1.substitute_in_place(variable, expression);
        exp2.substitute_in_place(variable, expression);
      }
      Expression::Comparison(_, left, right) => {
        left.substitute_in_place(variable, expression);
        right.substitute_in_place(variable, expression);
      }
      Expression::Condition(if_exp, then_exp, else_exp) => {
        if_exp.substitute_in_place(variable, expression);
        then_exp.substitute_in_place(variable, expression);
        else_exp.substitute_in_place(variable, expression);
      }
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FunctionType {
  Add,
  Subtract,
  Multiply,
  Divide,
  Exponentiation,
  Root,
  // Logarithm,
  Custom(String),
}

impl std::fmt::Display for FunctionType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      FunctionType::Add => write!(f, "+"),
      FunctionType::Subtract => write!(f, "-"),
      FunctionType::Multiply => write!(f, "*"),
      FunctionType::Divide => write!(f, "/"),
      FunctionType::Exponentiation => write!(f, "^"),
      FunctionType::Custom(name) => write!(f, "{}(...)", name),
      FunctionType::Root => write!(f, "root"),
      // FunctionType::Logarithm => write!(f, "log"),
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ComparisonType {
  Equal,
  NotEqual,
  LessThan,
  GreaterThan,
  LessThanOrEqual,
  GreaterThanOrEqual,
}

impl std::fmt::Display for ComparisonType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ComparisonType::Equal => write!(f, "="),
      ComparisonType::NotEqual => write!(f, "!="),
      ComparisonType::LessThan => write!(f, "<"),
      ComparisonType::GreaterThan => write!(f, ">"),
      ComparisonType::LessThanOrEqual => write!(f, "<="),
      ComparisonType::GreaterThanOrEqual => write!(f, ">="),
    }
  }
}
