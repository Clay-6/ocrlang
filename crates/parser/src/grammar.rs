mod expr;
mod stmt;

use lexer::TokenKind;
use syntax::SyntaxKind;

use crate::parser::{marker::CompletedMarker, Parser};

pub(crate) fn root(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    while !p.at_end() {
        stmt::stmt(p);
    }

    m.complete(p, SyntaxKind::Root)
}
