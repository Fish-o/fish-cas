mod number;
mod parser;
mod tokenizer;
fn main() {
  let data = "1*((2-1))-3";
  let token_stream = tokenizer::tokenize(data);
  println!("{:#?}", token_stream);
  let res = parser::parse(token_stream).unwrap();
  println!("{:#?}", res);
}
