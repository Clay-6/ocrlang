mod token_kind;

use std::ops::Range;

use logos::Logos;

pub use token_kind::TokenKind;

#[derive(Debug)]
pub struct Lexer<'i> {
    inner: logos::Lexer<'i, TokenKind>,
}

#[derive(Debug, PartialEq)]
pub struct Token<'i> {
    pub kind: TokenKind,
    pub text: &'i str,
    pub range: Range<usize>,
}

impl<'i> Lexer<'i> {
    #[must_use]
    pub fn new(src: &'i str) -> Self {
        Self {
            inner: TokenKind::lexer(src),
        }
    }
}

impl<'i> Iterator for Lexer<'i> {
    type Item = Token<'i>;

    fn next(&mut self) -> Option<Self::Item> {
        let kind = self.inner.next()?.unwrap();
        let text = self.inner.slice();
        let range = self.inner.span();

        Some(Self::Item { kind, text, range })
    }
}
