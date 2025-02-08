pub(crate) mod marker;
pub(crate) mod parse_error;

use std::mem;

use lexer::{Token, TokenKind};
use syntax::SyntaxKind;

use crate::{event::Event, grammar, source::Source};

pub(crate) use parse_error::ParseError;

use self::marker::Marker;

const RECOVERY_SET: [TokenKind; 24] = [
    TokenKind::Const,
    TokenKind::Global,
    TokenKind::Array,
    TokenKind::Function,
    TokenKind::Endfunction,
    TokenKind::Procedure,
    TokenKind::Endprocedure,
    TokenKind::If,
    TokenKind::Then,
    TokenKind::Elseif,
    TokenKind::Else,
    TokenKind::Endif,
    TokenKind::Switch,
    TokenKind::Case,
    TokenKind::Default,
    TokenKind::Endswitch,
    TokenKind::For,
    TokenKind::To,
    TokenKind::Step,
    TokenKind::Next,
    TokenKind::While,
    TokenKind::Endwhile,
    TokenKind::Do,
    TokenKind::Return,
];

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
        grammar::root(&mut self);

        self.events
    }

    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::Placeholder);
        Marker::new(pos)
    }

    pub(crate) fn bump(&mut self) {
        self.expected_kinds.clear();
        self.source.next_token().unwrap();
        self.events.push(Event::AddToken);
    }

    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        self.expected_kinds.push(kind);

        self.peek() == Some(kind)
    }

    pub(crate) fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    pub(crate) fn at_newline(&mut self) -> bool {
        self.expected_kinds.push(TokenKind::Newline);

        self.source.at_newline()
    }

    pub(crate) fn error(&mut self) {
        let curr_tok = self.source.peek_token();

        let (found, range) = if let Some(Token { kind, range, .. }) = curr_tok {
            (Some(*kind), range.clone())
        } else {
            (None, self.source.last_token_range().unwrap())
        };

        self.events.push(Event::Error(ParseError {
            expected: mem::take(&mut self.expected_kinds),
            found,
            range,
        }));

        if !self.at_set(&RECOVERY_SET) && !self.at_end() {
            let m = self.start();
            self.bump();
            m.complete(self, SyntaxKind::Error);
        }
    }

    pub(crate) fn expect(&mut self, kind: TokenKind) {
        if self.at(kind) {
            self.bump();
        } else {
            self.error();
        }
    }

    pub(crate) fn at_set(&mut self, set: &[TokenKind]) -> bool {
        self.peek().is_some_and(|kind| set.contains(&kind))
    }

    pub(crate) fn peek_next(&mut self) -> Option<TokenKind> {
        self.source.peek_next_kind()
    }

    fn peek(&mut self) -> Option<TokenKind> {
        self.source.peek_kind()
    }
}
