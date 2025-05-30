use std::ops::Range;

use lexer::{Token, TokenKind};

pub(crate) struct Source<'t, 'input> {
    tokens: &'t [Token<'input>],
    cursor: usize,
}

impl<'t, 'input> Source<'t, 'input> {
    pub(crate) fn new(tokens: &'t [Token<'input>]) -> Self {
        Self { tokens, cursor: 0 }
    }

    pub(crate) fn next_token(&mut self) -> Option<&'t Token<'input>> {
        self.skip_trivia();

        let token = self.tokens.get(self.cursor)?;
        self.cursor += 1;

        Some(token)
    }

    pub(crate) fn peek_token(&mut self) -> Option<&Token> {
        self.skip_trivia();
        self.peek_token_raw()
    }

    pub(crate) fn peek_kind(&mut self) -> Option<TokenKind> {
        self.skip_trivia();
        self.peek_kind_raw()
    }

    pub(crate) fn peek_next_kind(&mut self) -> Option<TokenKind> {
        self.skip_trivia();
        let mut next = self.peek_next_kind_raw();
        let mut i = self.cursor + 1;
        while next.is_some_and(lexer::TokenKind::is_trivia) {
            i += 1;
            next = self.tokens.get(i).map(|Token { kind, .. }| *kind);
        }
        next
    }

    pub(crate) fn last_token_range(&self) -> Option<Range<usize>> {
        self.tokens.last().map(|Token { range, .. }| range.clone())
    }

    pub(crate) fn at_newline(&mut self) -> bool {
        while !matches!(self.peek_kind_raw(), Some(TokenKind::Newline))
            && self.at_trivia()
        {
            self.cursor += 1;
        }
        self.peek_kind_raw()
            .is_some_and(|kind| matches!(kind, TokenKind::Newline))
    }

    fn skip_trivia(&mut self) {
        while self.at_trivia() {
            self.cursor += 1;
        }
    }

    fn at_trivia(&self) -> bool {
        self.peek_kind_raw().is_some_and(TokenKind::is_trivia)
    }

    fn peek_kind_raw(&self) -> Option<TokenKind> {
        self.peek_token_raw().map(|Token { kind, .. }| *kind)
    }

    fn peek_token_raw(&self) -> Option<&Token> {
        self.tokens.get(self.cursor)
    }

    fn peek_next_token_raw(&self) -> Option<&Token> {
        self.tokens.get(self.cursor + 1)
    }

    fn peek_next_kind_raw(&self) -> Option<TokenKind> {
        self.peek_next_token_raw().map(|Token { kind, .. }| *kind)
    }
}
