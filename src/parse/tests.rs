use nom::error::Error;

use super::*;

#[test]
fn test_assignment() {
    println!("{:?}", statement::statement::<Error<&str>>("s := x;"));
}
