// use std::fmt;
// #[derive(Clone, Copy)]
// pub struct Number {
//   numerator: u64,
//   denominator: u64,
//   negative: bool,
// }

// impl Number {
//   pub fn from_int(int: i32) -> Number {
//     Number {
//       numerator: int.abs() as u64,
//       denominator: 1,
//       negative: int.is_negative(),
//     }
//   }
//   pub fn from_string(string: &String) -> Number {
//     let numerator = string.parse::<u64>().expect("Invalid number conversion");
//     Number {
//       numerator,
//       denominator: 1,
//       negative: false,
//     }
//   }
// }

// // impl debug
// impl fmt::Debug for Number {
//   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//     write!(f, "Number[")?;
//     if self.negative {
//       write!(f, "-")?;
//     }
//     write!(f, "{}", self.numerator)?;
//     if self.denominator > 1 {
//       write!(f, "/{}", self.denominator)?;
//     }
//     write!(f, "]")
//   }
// }

// // Add
// impl std::ops::Add for Number {
//   type Output;

//   fn add(self, rhs: Self) -> Self::Output {

//   }
// }
