use num::{BigRational, FromPrimitive};

use crate::tokenizer::{Operator, Token};
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
        std::mem::replace(&mut tokens[index], Token::Expression(expr));
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
        std::mem::replace(&mut tokens[index], Token::Expression(expr));
        tokens.remove(index + 1);
        length -= 1;
      }
      (Token::Negate, Token::Identifier(ident)) => {
        let mut should_negate = true;
        let mut identifier = ident.clone();
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
              std::mem::replace(&mut tokens[index], Token::Expression(multiply_expr));
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
          std::mem::replace(&mut tokens[index], Token::Expression(expr));
          tokens.remove(index + 1);
          length -= 1;
        }
      }

      _ => {}
    }
    index += 1;
  }

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

#[derive(Debug, Clone)]
pub enum ExtendedParserError {
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
#[derive(Debug, Clone)]
pub enum Expression {
  Value(BigRational),
  Variable(String),
  Function(FunctionType, Vec<Expression>),
  Assignment(Box<Expression>, Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum FunctionType {
  Add,
  Subtract,
  Multiply,
  Divide,
  Exponentiation,
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
    }
  }
}
