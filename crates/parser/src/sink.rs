use lexer::Token;
use rowan::GreenNodeBuilder;

use crate::{event::Event, parser::parse_error::ParseError, Parse};

pub(crate) struct Sink<'t, 'input> {
    builder: GreenNodeBuilder<'static>,
    tokens: &'t [Token<'input>],
    cursor: usize,
    events: Vec<Event>,
    errors: Vec<ParseError>,
}

impl<'t, 'input> Sink<'t, 'input> {
    pub(crate) fn new(tokens: &'t [Token<'input>], events: Vec<Event>) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            tokens,
            cursor: 0,
            events,
            errors: vec![],
        }
    }

    pub(crate) fn finish(mut self) -> Parse {
        todo!()
    }
}
