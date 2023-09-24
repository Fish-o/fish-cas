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
  // let number = "-31/100";
  // let rational = BigRational::from_str(number);
  // println!("{:?}", rational);

  let data = "f(x)=x<3==false;f(2);f(3)";

  let token_stream = tokenizer::tokenize(data).unwrap();
  println!("{:#?}", token_stream);
  let expressions = parser::parse(token_stream).unwrap();
  println!("{:#?}", expressions);
  let evaluated = evaluator::evaluate(&expressions).unwrap();
  println!("{:#?}", evaluated);
}
