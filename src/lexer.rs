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
