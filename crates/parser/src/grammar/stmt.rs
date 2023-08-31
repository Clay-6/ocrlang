use super::*;

pub(crate) fn stmt(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(TokenKind::Ident) {
        todo!()
    } else {
        expr::expr(p)
    }
}
