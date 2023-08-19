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
}
