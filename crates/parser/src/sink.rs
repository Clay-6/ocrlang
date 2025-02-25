use std::mem;

use lexer::Token;
use rowan::{GreenNodeBuilder, Language};
use syntax::OcrLang;

use crate::{Parse, event::Event, parser::ParseError};

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
        for idx in 0..self.events.len() {
            match mem::replace(&mut self.events[idx], Event::Placeholder) {
                Event::StartNode {
                    kind,
                    forward_parent,
                } => {
                    let mut kinds = vec![kind];

                    let mut idx = idx;
                    let mut forward_parent = forward_parent;

                    while let Some(fp) = forward_parent {
                        idx += fp;

                        // Traverse along the `forward_parent` chain until we can't anymore
                        forward_parent = match mem::replace(
                            &mut self.events[idx],
                            Event::Placeholder,
                        ) {
                            Event::StartNode {
                                kind,
                                forward_parent,
                            } => {
                                kinds.push(kind);
                                forward_parent
                            }
                            _ => {
                                unreachable!(
                                    "The forward parent will always be a `StartNode`"
                                );
                            }
                        };
                    }

                    for kind in kinds.into_iter().rev() {
                        self.builder.start_node(OcrLang::kind_to_raw(kind));
                    }
                }
                Event::AddToken => self.token(),
                Event::FinishNode => self.builder.finish_node(),
                Event::Error(e) => self.errors.push(e),
                Event::Placeholder => {}
            }

            self.skip_trivia();
        }

        Parse {
            green_node: self.builder.finish(),
            errors: self.errors,
        }
    }

    fn token(&mut self) {
        let Token { kind, text, .. } = self.tokens[self.cursor];

        self.builder.token(OcrLang::kind_to_raw(kind.into()), text);

        self.cursor += 1;
    }

    fn skip_trivia(&mut self) {
        while let Some(token) = self.tokens.get(self.cursor) {
            if !token.kind.is_trivia() {
                break;
            }

            self.token();
        }
    }
}
