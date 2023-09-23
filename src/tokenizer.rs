use std::fmt::Write;

use crate::{number::Number, parser::Expression};

pub fn tokenize(text: &str) -> Vec<Token> {
  let mut char_iter = text.chars().peekable();
  let mut token_stream: Vec<Token> = Vec::new();
  while let Some(char) = &char_iter.next() {
    let token = match *char {
      '(' => Token::BracketOpen,
      ')' => Token::BracketClose,
      '+' => Token::Operator(Operator::Add),
      '-' => Token::Operator(Operator::Subtract),
      '*' => Token::Operator(Operator::Multiply),
      '/' => Token::Operator(Operator::Divide),
      '^' => Token::Operator(Operator::Power),
      'A'..='Z' | 'a'..='z' => {
        let mut identifier = "".to_owned();
        identifier.write_char(*char).expect("Failed to write char");
        while let Some(peeked) = char_iter.peek() {
          if match peeked {
            'A'..='Z' | 'a'..='z' | '_' | '0'..='9' => true,
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
        Token::Identifier(identifier)
      }
      '0'..='9' => {
        let mut number_chars = "".to_owned();
        number_chars
          .write_char(*char)
          .expect("Failed to write char");
        while let Some(peeked) = &char_iter.peek() {
          if match peeked {
            '0'..='9' => true,
            _ => false,
          } {
            number_chars
              .write_char(char_iter.next().expect("New char of number not existent"))
              .expect("Failed to write char");
          } else {
            break;
          }
        }
        Token::Number(Number::from_string(&number_chars))
      }
      _ => panic!("Token not found"),
    };
    token_stream.push(token)
  }
  token_stream
}

#[derive(Debug, Clone)]
pub enum Token {
  BracketOpen,
  BracketClose,
  Number(Number),
  Identifier(String),
  Operator(Operator),
  Batch(Box<Vec<Token>>),
  Expression(Expression),
}

#[derive(Debug, Clone)]
pub enum Operator {
  Add,
  Subtract,
  Multiply,
  Divide,
  Power,
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
