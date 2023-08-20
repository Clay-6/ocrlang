pub(crate) mod parse_error;

use lexer::TokenKind;

use crate::{event::Event, source::Source};

pub(crate) struct Parser<'t, 'input> {
    source: Source<'t, 'input>,
    events: Vec<Event>,
    expected_kinds: Vec<TokenKind>,
}
impl<'t, 'input> Parser<'t, 'input> {
    pub(crate) fn new(source: Source<'t, 'input>) -> Self {
        Self {
            source,
            events: vec![],
            expected_kinds: vec![],
        }
    }

    pub(crate) fn parse(mut self) -> Vec<Event> {
        todo!()
    }
}
