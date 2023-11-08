use core::fmt;
use std::ops::Range;

use lexer::TokenKind;
use rowan::{TextRange, TextSize};

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub struct ParseError {
    pub(super) expected: Vec<TokenKind>,
    pub(super) found: Option<TokenKind>,
    pub(super) range: Range<usize>,
}

impl ParseError {
    pub fn text_range(&self) -> Option<TextRange> {
        Some(TextRange::new(
            TextSize::new(self.range.start.try_into().ok().or(None)?),
            TextSize::new(self.range.end.try_into().ok().or(None)?),
        ))
    }

    pub fn context(&self) -> String {
        use std::fmt::Write;

        let mut s = String::new();
        write!(s, "expected ").unwrap();
        let num_expected = self.expected.len();
        let is_first = |i| i == 0;
        let is_last = |i| i == num_expected - 1;

        for (i, expected) in self.expected.iter().enumerate() {
            if is_first(i) {
                write!(s, "{expected}").unwrap();
            } else if is_last(i) {
                write!(s, ", or {expected}").unwrap();
            } else {
                write!(s, ", {expected}").unwrap();
            }
        }

        if let Some(found) = self.found {
            write!(s, ", but found {found}").unwrap();
        }

        s
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "error at {}..{}: {}",
            self.range.start,
            self.range.end,
            self.context()
        )?;

        Ok(())
    }
}
