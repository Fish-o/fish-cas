use num::BigRational;

use crate::tokenizer::{Operator, Token};
pub fn parse(token_stream: Vec<Token>) -> Result<Expression, ParserError> {
  let batched_tokens = batch(&token_stream).expect("Failed to batch tokens");
  let parsed_token = parse_batches(batched_tokens)?;
  Ok(parsed_token)
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

  let mut tokens = tokens;
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
    Token::Identifier(identifier) => Expression::Variable(identifier.clone()),
    Token::Operator(operator) => {
      return Err(ParserError::UnexpectedToken(format!(
        "Unexpected operator '{}'",
        operator
      )))
    }
    Token::Batch(tokens) => parse_batches(tokens.to_vec())?,
    Token::Expression(expression) => expression.clone(),
    _ => unreachable!(),
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
#[derive(Debug)]
pub enum ParserError {
  BatchingError(String),
  LeftHandMissing(String),
  RightHandMissing(String),
  UnexpectedToken(String),
  UnknownParserError(String),
}
#[derive(Debug, Clone)]
pub enum Expression {
  Value(BigRational),
  Variable(String),
  Function(FunctionType, Vec<Expression>),
}

#[derive(Debug, Clone)]
pub enum FunctionType {
  Add,
  Subtract,
  Multiply,
  Divide,
  Exponentiation,
  Sine,
}

impl std::fmt::Display for FunctionType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      FunctionType::Add => write!(f, "+"),
      FunctionType::Subtract => write!(f, "-"),
      FunctionType::Multiply => write!(f, "*"),
      FunctionType::Divide => write!(f, "/"),
      FunctionType::Exponentiation => write!(f, "^"),
      FunctionType::Sine => write!(f, "sin"),
    }
  }
}
