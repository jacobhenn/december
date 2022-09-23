//! an interpreter for December, an imperative, strongly-typed programming language that i am
//! making for fun.

#[macro_use]
pub mod error;

pub mod args;
pub mod parse;
pub mod run;
pub mod types;

use crate::args::Args;
use anyhow::{Context, Result};
use nom::{error::VerboseError, Err};
use std::{
    fs::File,
    io::{BufReader, Read},
};

fn main() -> Result<()> {
    let args: Args = argh::from_env();
    let file = File::open(args.path).context("couldn't open source file")?;
    let mut buf_reader = BufReader::new(file);
    let mut contents = String::new();
    buf_reader
        .read_to_string(&mut contents)
        .context("couldn't read source file")?;

    match parse::program::<VerboseError<&str>>(&contents) {
        Ok((_, program)) => {
            if let Err(e) = run::program(program) {
                println!("Runtime error: {e:#}");
            }
        }
        Err(Err::Error(e) | Err::Failure(e)) => {
            println!("{}", error::parse::contextualize(contents.as_str(), &e));
        }
        _ => (),
    }

    Ok(())
}
