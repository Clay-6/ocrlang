use super::*;

pub(crate) fn expr(p: &mut Parser) -> Option<CompletedMarker> {
    expr_bp(p)
}

fn expr_bp(p: &mut Parser) -> Option<CompletedMarker> {
    todo!()
}
enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Quot,
    Pow,
}

enum PrefixOp {
    Neg,
}

impl InfixOp {
    fn bp(&self) -> (u8, u8) {
        match self {
            Self::Add | Self::Sub => (1, 2),
            Self::Mul | Self::Div => (3, 4),
            Self::Mod | Self::Quot => (5, 6),
            Self::Pow => (9, 8),
        }
    }
}

impl PrefixOp {
    fn bp(&self) -> ((), u8) {
        match self {
            PrefixOp::Neg => ((), 7),
        }
    }
}
