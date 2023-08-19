use logos::Logos;

#[derive(Debug, PartialEq, Logos)]
pub enum Token {
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("^")]
    Caret,
    #[token(":")]
    Colon,
    #[token("MOD")]
    Mod,
    #[token("DIV")]
    Div,
    #[token("AND")]
    And,
    #[token("OR")]
    Or,
    #[token("NOT")]
    Not,
    #[token("==")]
    EqualEqual,
    #[token("!=")]
    BangEqual,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEqual,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEqual,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,

    #[token("const")]
    Const,
    #[token("global")]
    Global,
    #[token("=")]
    Equal,
    #[regex("[A-Za-z_][A-Za-z0-9_]*")]
    Ident,

    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("elseif")]
    Elseif,
    #[token("else")]
    Else,
    #[token("endif")]
    Endif,
    #[token("switch")]
    Switch,
    #[token("case")]
    Case,
    #[token("default")]
    Default,
    #[token("endswitch")]
    Endswitch,

    #[token("\n")]
    Newline,
    #[regex(r"[ \t\r\f]+")]
    Whitespace,
    #[regex("//.*")]
    Comment,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(src: &str, expected: Token) {
        let mut lex = Token::lexer(src);

        assert_eq!(lex.next().unwrap().unwrap(), expected);
        assert_eq!(lex.slice(), src);
    }

    #[test]
    fn lex_plus() {
        check("+", Token::Plus);
    }

    #[test]
    fn lex_minus() {
        check("-", Token::Minus);
    }

    #[test]
    fn lex_star() {
        check("*", Token::Star);
    }

    #[test]
    fn lex_slash() {
        check("/", Token::Slash);
    }

    #[test]
    fn lex_caret() {
        check("^", Token::Caret)
    }

    #[test]
    fn lex_colon() {
        check(":", Token::Colon);
    }

    #[test]
    fn lex_mod() {
        check("MOD", Token::Mod);
    }

    #[test]
    fn lex_div() {
        check("DIV", Token::Div);
    }

    #[test]
    fn lex_and() {
        check("AND", Token::And);
    }

    #[test]
    fn lex_or() {
        check("OR", Token::Or);
    }

    #[test]
    fn lex_not() {
        check("NOT", Token::Not);
    }

    #[test]
    fn lex_const() {
        check("const", Token::Const);
    }

    #[test]
    fn lex_global() {
        check("global", Token::Global);
    }

    #[test]
    fn lex_equal() {
        check("=", Token::Equal);
    }

    #[test]
    fn lex_ident() {
        check("an_ident123", Token::Ident);
    }

    #[test]
    fn lex_equal_equal() {
        check("==", Token::EqualEqual);
    }

    #[test]
    fn lex_less() {
        check("<", Token::Less);
    }

    #[test]
    fn lex_less_equal() {
        check("<=", Token::LessEqual);
    }

    #[test]
    fn lex_greater() {
        check(">", Token::Greater);
    }

    #[test]
    fn lex_greater_equal() {
        check(">=", Token::GreaterEqual);
    }

    #[test]
    fn lex_bang_equal() {
        check("!=", Token::BangEqual);
    }

    #[test]
    fn lex_parens() {
        check("(", Token::LParen);
        check(")", Token::RParen);
    }

    #[test]
    fn lex_brackets() {
        check("[", Token::LBracket);
        check("]", Token::RBracket)
    }

    #[test]
    fn lex_if() {
        check("if", Token::If)
    }

    #[test]
    fn lex_then() {
        check("then", Token::Then);
    }

    #[test]
    fn lex_elseif() {
        check("elseif", Token::Elseif)
    }

    #[test]
    fn lex_else() {
        check("else", Token::Else)
    }

    #[test]
    fn lex_endif() {
        check("endif", Token::Endif)
    }

    #[test]
    fn lex_switch() {
        check("switch", Token::Switch);
    }

    #[test]
    fn lex_case() {
        check("case", Token::Case);
    }

    #[test]
    fn lex_default() {
        check("default", Token::Default);
    }

    #[test]
    fn lex_endswitch() {
        check("endswitch", Token::Endswitch);
    }

    #[test]
    fn lex_comment() {
        check("// A comment", Token::Comment);
    }

    #[test]
    fn lex_whitespace() {
        check("  \t\r", Token::Whitespace);
    }

    #[test]
    fn lex_newline() {
        check("\n", Token::Newline);
    }
}
