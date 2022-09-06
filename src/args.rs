use argh::FromArgs;
use std::path::PathBuf;

#[derive(FromArgs)]
/// an interpreter for the December programming language
pub struct Args {
    /// path to the main source file
    #[argh(positional)]
    pub path: PathBuf,
}
