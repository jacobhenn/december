use argh::FromArgs;
use std::path::PathBuf;

#[derive(FromArgs)]
/// an interpreter for the December programming language
pub struct Args {
    /// path to the main source file
    #[argh(positional)]
    pub path: PathBuf,

    /// pretty-print the ast as a Rust object before running the program
    #[argh(switch)]
    pub print_ast: bool,
}
