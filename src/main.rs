mod args;
mod parse;
mod run;
mod types;

use crate::args::Args;
use anyhow::{Context, Result};
use nom::{
    error::{convert_error, VerboseError},
    Err,
};
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
            println!("parsed complete program:");
            println!("{program:#?}");
            println!("running program:");
            run::run(program)?;
        }
        Err(Err::Error(e) | Err::Failure(e)) => {
            println!("syntax error");
            println!("{}", convert_error(contents.as_str(), e));
        }
        _ => (),
    }

    Ok(())
}
