use lexer::TokenKind;

use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, FromPrimitive, ToPrimitive)]
pub enum SyntaxKind {
    Plus,
    Minus,
    Star,
    Slash,
    Caret,
    Colon,
    Mod,
    Div,
    And,
    Or,
    Not,
    EqualEqual,
    BangEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Dot,
    Comma,
    Const,
    Global,
    Array,
    Equal,
    Ident,
    If,
    Then,
    Elseif,
    Else,
    Endif,
    Switch,
    Case,
    Default,
    Endswitch,
    For,
    To,
    Step,
    Next,
    Do,
    Until,
    While,
    Endwhile,
    Function,
    Return,
    Endfunction,
    Procedure,
    Endprocedure,
    String,
    Char,
    Number,
    True,
    False,
    Newline,
    Whitespace,
    Comment,

    Root,
    Error,

    BinaryOp,
    Literal,
    ArrayLiteral,
    ParenExpr,
    UnaryExpr,
    NameRef,
    BinaryExpr,
    SubprogCall,
    IdentSubscript,

    VarDef,
    SubProgramDef,
    RetStmt,
    IfStmt,
    ForLoop,
    WhileLoop,
    DoUntil,
    SwitchStmt,
}

pub type SyntaxNode = rowan::SyntaxNode<OcrLang>;
pub type SyntaxElement = rowan::SyntaxElement<OcrLang>;
pub type SyntaxToken = rowan::SyntaxToken<OcrLang>;

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct OcrLang;

impl rowan::Language for OcrLang {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        Self::Kind::from_u16(raw.0).unwrap()
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.to_u16().unwrap())
    }
}

impl From<TokenKind> for SyntaxKind {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Plus => Self::Plus,
            TokenKind::Minus => Self::Minus,
            TokenKind::Star => Self::Star,
            TokenKind::Slash => Self::Slash,
            TokenKind::Caret => Self::Caret,
            TokenKind::Colon => Self::Colon,
            TokenKind::Mod => Self::Mod,
            TokenKind::Div => Self::Div,
            TokenKind::And => Self::And,
            TokenKind::Or => Self::Or,
            TokenKind::Not => Self::Not,
            TokenKind::EqualEqual => Self::EqualEqual,
            TokenKind::BangEqual => Self::BangEqual,
            TokenKind::Greater => Self::Greater,
            TokenKind::GreaterEqual => Self::GreaterEqual,
            TokenKind::Less => Self::Less,
            TokenKind::LessEqual => Self::LessEqual,
            TokenKind::LParen => Self::LParen,
            TokenKind::RParen => Self::RParen,
            TokenKind::LBracket => Self::LBracket,
            TokenKind::RBracket => Self::RBracket,
            TokenKind::Dot => Self::Dot,
            TokenKind::Comma => Self::Comma,
            TokenKind::Const => Self::Const,
            TokenKind::Global => Self::Global,
            TokenKind::Array => Self::Array,
            TokenKind::Equal => Self::Equal,
            TokenKind::Ident => Self::Ident,
            TokenKind::If => Self::If,
            TokenKind::Then => Self::Then,
            TokenKind::Elseif => Self::Elseif,
            TokenKind::Else => Self::Else,
            TokenKind::Endif => Self::Endif,
            TokenKind::Switch => Self::Switch,
            TokenKind::Case => Self::Case,
            TokenKind::Default => Self::Default,
            TokenKind::Endswitch => Self::Endswitch,
            TokenKind::For => Self::For,
            TokenKind::To => Self::To,
            TokenKind::Step => Self::Step,
            TokenKind::Next => Self::Next,
            TokenKind::Do => Self::Do,
            TokenKind::Until => Self::Until,
            TokenKind::While => Self::While,
            TokenKind::Endwhile => Self::Endwhile,
            TokenKind::Function => Self::Function,
            TokenKind::Return => Self::Return,
            TokenKind::Endfunction => Self::Endfunction,
            TokenKind::Procedure => Self::Procedure,
            TokenKind::Endprocedure => Self::Endprocedure,
            TokenKind::String => Self::String,
            TokenKind::Char => Self::Char,
            TokenKind::Number => Self::Number,
            TokenKind::True => Self::True,
            TokenKind::False => Self::False,
            TokenKind::Newline => Self::Newline,
            TokenKind::Whitespace => Self::Whitespace,
            TokenKind::Comment => Self::Comment,
        }
    }
}
