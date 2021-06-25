//! Program synthesis of string transformations from input-output examples.
//!
//! This crate implements program synthesis of string transformations from input-output examples.
//! Perhaps the most well-known use of string program synthesis in end-user programs is the [Flash
//! Fill](https://support.microsoft.com/en-us/office/using-flash-fill-in-excel-3f9bcf1e-db93-4890-94a0-1578341f73f7)
//! feature in Excel. These string transformations are learned from input-output examples. In some
//! cases, extra unpaired input examples can be used to improve results even when the outputs
//! corresponding to those examples are not available.
//!
//! String programs take a row of columns (strings) and produce a single string as an output. For
//! example, a string program can capture the transformation demonstrated in the following table:
//!
//! | Name | Graduation Year | Output |
//! |---|---|---|
//! | Alyssa P. Hacker | 1985 | A. Hacker '85 |
//! | Ben Bitdiddle | 2002 | B. Bitdiddle '02 |
//! | Cy D. Fect | 2017 | ? |
//!
//! The following example shows how to use the [blinkfill] module to learn a program that learns
//! the above transformation and apply it to an example with unknown output (the last row in the
//! table).
//!
//! ```
//! use synox::StringProgram;
//! use synox::blinkfill;
//!
//! # fn main() -> Result<(), ()> {
//! #   assert_eq!(example(), Some(()));
//! #   Ok(())
//! # }
//! # fn example() -> Option<()> {
//! // no unpaired examples
//! let unpaired: &[Vec<&str>] = &[];
//! // two paired examples, demonstrating a diversity of inputs (one including a middle initial)
//! let examples = &[(vec!["Alyssa P. Hacker", "1985"], "A. Hacker '85"   ),
//!                  (vec!["Ben Bitdiddle",    "2002"], "B. Bitdiddle '02")];
//!
//! // learn a program based on input-output examples
//! //
//! // blinkfill::learn returns an Option because it may fail to learn a program that matches all
//! // the examples
//! let prog = blinkfill::learn(unpaired, examples)?;
//!
//! // StringProgram::run returns an Option because the program may fail on a particular input
//! let result = prog.run(&["Cy D. Fect", "2017"])?;
//! assert_eq!(result, "C. Fect '17");
//! # Some(())
//! # }
//! ```
//!
//! To handle multiple output columns, you can infer separate string programs, one for each output
//! column.

#![doc(html_root_url = "https://docs.rs/synox/0.1.0")]
#![warn(missing_docs)]

pub mod blinkfill;
mod graph;

/// A program that transforms a list of strings into a string.
///
/// This trait is sealed and not meant to be implemented outside this crate.
pub trait StringProgram: private::Sealed {
    /// Runs the program on the given list of strings.
    ///
    /// The string program might error on a particular input. For example, if the string program is
    /// extracting the third word in a column, but the column has only two words, this will result
    /// in an error. In case of an error, this function returns `None`. String programs do not
    /// return a more expressive error type because the programs are not manually written but
    /// inferred from input-output examples, so reporting details of errors in the auto-generated
    /// programs is unlikely to be useful to the end-user.
    fn run<S: AsRef<str>>(&self, row: &[S]) -> Option<String>;
}

mod private {
    pub trait Sealed {}
}
