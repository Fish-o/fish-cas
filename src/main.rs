mod evaluator;
mod parser;
mod tokenizer;
/**
 * TODO:
 * - Tokenizing negatives and decimals
 * - Add implicit multiplication in scenarios like 5a or 2(a+b)
 * - Create custom functions
 * - Have exact roots: sqrt(50) = 5sqrt(2)
 * - Graphing
 * - Logarithms
 * - Combinatorics
 * - Logical
 */
fn main() {
  let data = "26^(1/2)";

  let token_stream = tokenizer::tokenize(data);
  println!("{:#?}", token_stream);
  let exp = parser::parse(token_stream).unwrap();
  println!("{:#?}", exp);
  let evaluated = evaluator::evaluate(&exp).unwrap();
  println!("{:#?}", evaluated);
}
