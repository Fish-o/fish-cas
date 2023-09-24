use colored::Colorize;
use std::{env, io::Write};

mod evaluator;
mod parser;
mod state;
mod tokenizer;
/**
 * TODO:
 * | Tokenizing negatives and decimals
 * | Add implicit multiplication in scenarios like 5a or 2(a+b)
 * | Create custom functions
 * | Add booleans
 * | Add comparators
 * | Add cases for functions
 * - Have exact roots: sqrt(50) = 5sqrt(2)
 * - Graphing
 * - Logarithms
 * - Combinatorics
 * - Logical
 */

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let args: Vec<String> = env::args().collect();
  // let wd = std::env::current_dir()?;
  if args.len() != 2 {
    println!("Welcome to the fish-cas calculator!");
    println!("Currently running in REPL mode.");
    println!("To run a file, run file with the file path as the first argument.");
    println!("Example: fish-cas ./examples/1.fish");
    println!("To exit, type 'exit' or 'quit'.");
    println!("You can clear all state by running 'clear' or 'reset'.");
    let mut state = state::State::new();
    let mut execution_count = 1;
    loop {
      print!("{} >>> ", execution_count);
      let mut input = String::new();
      std::io::stdout().flush()?;
      std::io::stdin().read_line(&mut input)?;
      let input = input.trim();
      if input == "exit" || input == "quit" {
        println!("Goodbye!");
        break;
      } else if input == "clear" || input == "reset" {
        state.clear();
        continue;
      }

      let token_stream = tokenizer::tokenize(input);
      if token_stream.is_err() {
        println!("Error reading tokens: {:?}", token_stream.err().unwrap());
        continue;
      }
      let expressions = parser::parse(token_stream.unwrap());
      if expressions.is_err() {
        println!(
          "Error parsing expressions: {:?}",
          expressions.err().unwrap()
        );
        continue;
      }
      for exp in expressions.unwrap().iter() {
        let evaluated = exp.evaluate(&mut state).unwrap().to_string().bold();
        println!("{execution_count}: {evaluated}");
        execution_count += 1;
      }
    }
  } else {
    let path = std::path::Path::new(&args[1]);
    if !path.exists() {
      println!("File '{}' does not exist", path.display());
      return Ok(());
    }
    let file = std::fs::read_to_string(path)?;
    let input = file.as_str();
    let token_stream = tokenizer::tokenize(input).unwrap();
    let expressions = parser::parse(token_stream).unwrap();
    let evaluated = evaluator::evaluate(&expressions).unwrap();
    for (index, res) in evaluated.iter().enumerate() {
      let orig_exp = &expressions[index];
      let index = index + 1;
      let orig_exp = orig_exp.to_string().italic();
      let res = res.to_string().bold();
      println!("{index}: {orig_exp} => {res}");
    }
  }

  Ok(())
}
