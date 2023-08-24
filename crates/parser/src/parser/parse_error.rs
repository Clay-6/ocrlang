use core::fmt;
use std::ops::Range;

use lexer::TokenKind;

#[derive(Debug, PartialEq)]
pub(crate) struct ParseError {
    pub(super) expected: Vec<TokenKind>,
    pub(super) found: Option<TokenKind>,
    pub(super) range: Range<usize>,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "error at {}..{}: expected ",
            self.range.start, self.range.end
        )?;

        let num_expected = self.expected.len();
        let is_first = |i| i == 0;
        let is_last = |i| i == num_expected - 1;

        for (i, expected) in self.expected.iter().enumerate() {
            if is_first(i) {
                write!(f, "{}", expected)?;
            } else if is_last(i) {
                write!(f, ", or {}", expected)?;
            } else {
                write!(f, ", {}", expected)?;
            }
        }

        if let Some(found) = self.found {
            write!(f, ", but found {}", found)?;
        }

        Ok(())
    }
}
