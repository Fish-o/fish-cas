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
 * - Add cases for functions
 * - Have exact roots: sqrt(50) = 5sqrt(2)
 * - Graphing
 * - Logarithms
 * - Combinatorics
 * - Logical
 */

fn main() {
  let input = "
  f(x)= if x<=1 then 1 else f(x-1) + f(x-2);
  f(0);
  f(1);
  f(2);
  f(3);
  f(4);
  f(5)";

  let token_stream = tokenizer::tokenize(input).unwrap();
  let expressions = parser::parse(token_stream).unwrap();
  let evaluated = evaluator::evaluate(&expressions).unwrap();
  for (index, res) in evaluated.iter().enumerate() {
    let orig_exp = &expressions[index];
    let index = index + 1;
    println!("{index}: {orig_exp} -> {res}")
  }
}
