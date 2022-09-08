use nom::{
    error::{VerboseError, VerboseErrorKind},
    Offset,
};
use std::fmt::Write;

/// adapted for my purposes from
/// [`nom::error::convert_error`](https://docs.rs/nom/7.1.1/src/nom/error.rs.html#251-360)
pub fn contextualize<I: core::ops::Deref<Target = str>>(input: I, e: &VerboseError<I>) -> String {
    let mut result = String::new();

    if let Some((substring, VerboseErrorKind::Context(c))) = e
        .errors
        .iter()
        .find(|(_, k)| matches!(k, VerboseErrorKind::Context(..)))
    {
        let offset = input.offset(substring);

        if input.is_empty() {
            write!(&mut result, "expected {}, got empty input", c).unwrap();
        } else {
            let prefix = &input.as_bytes()[..offset];

            // count the number of newlines in the first `offset` bytes of input
            let line_number = bytecount::count(prefix, b'\n');

            // find the line that includes the subslice:
            // find the *last* newline before the substring starts
            let line_begin = prefix
                .iter()
                .rev()
                .position(|&b| b == b'\n')
                .map_or(0, |pos| offset - pos);

            // find the full line after that newline
            let line = input[line_begin..]
                .lines()
                .next()
                .unwrap_or(&input[line_begin..])
                .trim_end();

            // the (1-indexed) column number is the offset of our substring into that line
            let column_number = line.offset(substring) + 1;

            write!(
                &mut result,
                "syntax error: expected {expected}\n\
                         --> line {line_number}, column {column}\n\
                        {line}\n\
                        {caret:>column$}\n",
                expected = c,
                caret = '^',
                column = column_number,
            )
            .unwrap();
        }
    }

    // // for debugging purposes
    // println!("full trace:");
    // for error in e.errors.iter() {
    //     if let (_, VerboseErrorKind::Context(c)) = error {
    //         println!("  {}", c);
    //     }
    // }

    result
}
