mod expr;
mod stmt;

use lexer::TokenKind;
use syntax::SyntaxKind;

use crate::parser::{Parser, marker::CompletedMarker};

pub(crate) fn root(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    while !p.at_end() {
        stmt::stmt(p);
    }

    m.complete(p, SyntaxKind::Root)
}
