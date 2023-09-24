use std::{fmt::Write, ops::Neg, str::FromStr};

use num::BigRational;

use crate::parser::Expression;

fn convert_keyword(text: &str) -> Option<Token> {
  match text {
    "true" => Some(Token::Boolean(true)),
    "false" => Some(Token::Boolean(false)),
    "if" => Some(Token::Keyword(Keyword::If)),
    "then" => Some(Token::Keyword(Keyword::Then)),
    "else" => Some(Token::Keyword(Keyword::Else)),
    _ => None,
  }
}
pub fn tokenize(text: &str) -> Result<Vec<Token>, TokenError> {
  let mut char_iter = text.chars().peekable();
  let mut token_stream: Vec<Token> = Vec::new();
  while let Some(char) = &char_iter.next() {
    let token = match *char {
      ';' => Token::EndOfExpression,
      ',' => Token::Separator,
      '(' => Token::BracketOpen,
      ')' => Token::BracketClose,
      '+' => Token::Operator(Operator::Add),
      '-' => {
        if let Some(_) = char_iter.peek() {
          let last_token = token_stream.last();
          if last_token.is_none() {
            Token::Negate
          } else {
            let last_token = last_token.unwrap();
            if match last_token {
              Token::BracketOpen => true,
              Token::Comparator(_) => true,
              Token::Operator(_) => true,
              Token::Negate => true,
              Token::EndOfExpression => true,
              Token::Assignment => true,
              Token::Separator => true,
              Token::Keyword(_) => true,

              Token::Identifier(_) => false,
              Token::Batch(_) => false,
              Token::BracketClose => false,
              Token::Number(_) => false,
              Token::Expression(_) => false,
              Token::Boolean(_) => false,
            } {
              Token::Negate
            } else {
              Token::Operator(Operator::Subtract)
            }
          }
        } else {
          Token::Operator(Operator::Subtract)
        }
      }
      '*' => Token::Operator(Operator::Multiply),
      '/' => Token::Operator(Operator::Divide),
      '^' => Token::Operator(Operator::Power),
      '=' => {
        if let Some(token) = char_iter.peek() {
          match token {
            '=' => {
              char_iter.next();
              Token::Comparator(Comparator::Equal)
            }
            _ => Token::Assignment,
          }
        } else {
          Token::Assignment
        }
      }
      '>' => {
        if let Some(token) = char_iter.peek() {
          match token {
            '=' => {
              char_iter.next();
              Token::Comparator(Comparator::GreaterThanOrEqual)
            }
            _ => Token::Comparator(Comparator::GreaterThan),
          }
        } else {
          Token::Comparator(Comparator::GreaterThan)
        }
      }
      '<' => {
        if let Some(token) = char_iter.peek() {
          match token {
            '=' => {
              char_iter.next();
              Token::Comparator(Comparator::LessThanOrEqual)
            }
            _ => Token::Comparator(Comparator::LessThan),
          }
        } else {
          Token::Comparator(Comparator::LessThan)
        }
      }
      '!' => {
        if let Some(token) = char_iter.peek() {
          match token {
            '=' => {
              char_iter.next();
              Token::Comparator(Comparator::NotEqual)
            }
            _ => panic!("Token '!' not found"),
          }
        } else {
          panic!("Token '!' not found")
        }
      }
      'A'..='Z' | 'a'..='z' => {
        let mut identifier = "".to_owned();
        identifier.write_char(*char).expect("Failed to write char");
        while let Some(peeked) = char_iter.peek() {
          if match peeked {
            'A'..='Z' | 'a'..='z' | '_' => true,
            _ => false,
          } {
            identifier
              .write_char(
                char_iter
                  .next()
                  .expect("New char of identifier not existent"),
              )
              .expect("Failed to write char");
          } else {
            break;
          }
        }
        convert_keyword(&identifier).unwrap_or(Token::Identifier(identifier))
      }
      '0'..='9' | '.' | '_' => {
        let mut number_chars = "".to_owned();
        number_chars
          .write_char(*char)
          .expect("Failed to write char");
        while let Some(peeked) = &char_iter.peek() {
          if match peeked {
            '0'..='9' | '.' | '_' => true,
            _ => false,
          } {
            number_chars
              .write_char(char_iter.next().expect("New char of number not existent"))
              .expect("Failed to write char");
          } else {
            break;
          }
        }
        let mut number = "".to_owned();
        let mut decimal_place = -1 as i32;
        for char in number_chars.chars() {
          match char {
            '0'..='9' => {
              if decimal_place >= 0 {
                decimal_place += 1;
              }
              number += char.to_string().as_str();
            }
            '_' => {}
            '.' => {
              if decimal_place >= 0 {
                return Err(TokenError::InvalidNumber(
                  "To many decimals in one number".to_string(),
                ));
              } else {
                decimal_place = 0
              }
            }
            _ => unreachable!(),
          }
        }
        if decimal_place > 0 {
          number = number + "/1" + "0".repeat(decimal_place.try_into().unwrap()).as_str()
        }
        let num = BigRational::from_str(&number).expect("Failed to read number");
        Token::Number(num)
      }
      ' ' | '\n' | '\t' => continue,
      t => panic!("Token '{}' not found", t),
    };
    token_stream.push(token)
  }
  let length = token_stream.len() as usize;
  let mut index = length - 1;
  while index > 0 {
    let token = &token_stream[index];
    let before = &token_stream[index - 1];
    match token {
      Token::Number(n) => match before {
        Token::Negate => {
          let new_number = n.neg();
          let _ = std::mem::replace(&mut token_stream[index - 1], Token::Number(new_number));
          token_stream.remove(index);
        }
        _ => {}
      },
      _ => {}
    }
    index -= 1;
  }

  let mut length = token_stream.len();
  let mut index = 0;
  while index < length - 2 {
    let token = &token_stream[index];
    let next_token = &token_stream[index + 1];
    if match (token, next_token) {
      (Token::Identifier(_), Token::Number(_))
      | (Token::Number(_), Token::Identifier(_))
      | (Token::BracketClose, Token::Number(_))
      | (Token::BracketClose, Token::Identifier(_))
      | (Token::BracketClose, Token::BracketOpen)
      | (Token::Number(_), Token::BracketOpen) => true,
      (Token::Identifier(_), Token::BracketOpen) => false,
      _ => false,
    } {
      index += 1;
      token_stream.insert(index, Token::Operator(Operator::Multiply));
      length = token_stream.len();
    }
    index += 1;
  }
  Ok(token_stream)
}

#[derive(Debug, Clone)]
pub enum Token {
  EndOfExpression,
  BracketOpen,
  BracketClose,
  Negate,
  Number(BigRational),
  Boolean(bool),
  Identifier(String),
  Operator(Operator),
  Comparator(Comparator),
  Batch(Box<Vec<Token>>),
  Expression(Expression),
  Assignment,
  Separator,
  Keyword(Keyword),
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
  If,
  Then,
  Else,
}
impl std::fmt::Display for Keyword {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Keyword::If => write!(f, "if"),
      Keyword::Then => write!(f, "then"),
      Keyword::Else => write!(f, "else"),
    }
  }
}

#[derive(Debug, Clone)]
pub enum Operator {
  Add,
  Subtract,
  Multiply,
  Divide,
  Power,
}
#[derive(Debug, Clone)]
pub enum Comparator {
  Equal,
  NotEqual,
  LessThan,
  GreaterThan,
  LessThanOrEqual,
  GreaterThanOrEqual,
}
impl std::fmt::Display for Comparator {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Comparator::Equal => write!(f, "=="),
      Comparator::NotEqual => write!(f, "!="),
      Comparator::LessThan => write!(f, "<"),
      Comparator::GreaterThan => write!(f, ">"),
      Comparator::LessThanOrEqual => write!(f, "<="),
      Comparator::GreaterThanOrEqual => write!(f, ">="),
    }
  }
}

#[derive(Debug, Clone)]
pub enum TokenError {
  InvalidNumber(String),
}

impl std::fmt::Display for Operator {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Operator::Add => write!(f, "+"),
      Operator::Subtract => write!(f, "-"),
      Operator::Multiply => write!(f, "*"),
      Operator::Divide => write!(f, "/"),
      Operator::Power => write!(f, "^"),
    }
  }
}
