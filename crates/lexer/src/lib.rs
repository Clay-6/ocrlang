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
    type Item = Result<Token<'i>, (&'i str, Range<usize>)>;

    fn next(&mut self) -> Option<Self::Item> {
        let kind = self.inner.next()?;
        let text = self.inner.slice();
        let range = self.inner.span();

        if let Ok(kind) = kind {
            Some(Ok(Token { kind, text, range }))
        } else {
            Some(Err((text, range)))
        }
    }
}
