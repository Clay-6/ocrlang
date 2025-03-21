use core::fmt;

use logos::Logos;

#[derive(Debug, Clone, Copy, PartialEq, Logos)]
pub enum TokenKind {
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

    #[token(".")]
    Dot,
    #[token(",")]
    Comma,

    #[token("const")]
    Const,
    #[token("global")]
    Global,
    #[token("array")]
    Array,
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
    #[token("for")]
    For,
    #[token("to")]
    To,
    #[token("step")]
    Step,
    #[token("next")]
    Next,
    #[token("do")]
    Do,
    #[token("until")]
    Until,
    #[token("while")]
    While,
    #[token("endwhile")]
    Endwhile,

    #[token("function")]
    Function,
    #[token("return")]
    Return,
    #[token("endfunction")]
    Endfunction,
    #[token("procedure")]
    Procedure,
    #[token("endprocedure")]
    Endprocedure,
    #[token("import")]
    Import,

    #[regex(r#""[^"\\]*(\\.[^"\\]*)*""#)]
    String,
    #[regex(r#"'([^'\\\n]|\\.)'"#)]
    Char,
    #[regex(r"[0-9]+(\.[0-9]+)?")]
    Number,
    #[token("true")]
    True,
    #[token("false")]
    False,

    #[token("\n")]
    #[token("\r\n")]
    Newline,
    #[regex(r"[ \t\f]+")]
    Whitespace,
    #[regex("//.*")]
    Comment,
}

impl TokenKind {
    #[must_use]
    pub fn is_trivia(self) -> bool {
        matches!(
            self,
            TokenKind::Newline | TokenKind::Whitespace | TokenKind::Comment
        )
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            TokenKind::Plus => "`+`",
            TokenKind::Minus => "`-`",
            TokenKind::Star => "`*`",
            TokenKind::Slash => "`/`",
            TokenKind::Caret => "`^`",
            TokenKind::Colon => "`:`",
            TokenKind::Mod => "`MOD`",
            TokenKind::Div => "`DIV`",
            TokenKind::And => "`AND`",
            TokenKind::Or => "`OR`",
            TokenKind::Not => "`NOT`",
            TokenKind::EqualEqual => "`==`",
            TokenKind::BangEqual => "`!=`",
            TokenKind::Greater => "`>`",
            TokenKind::GreaterEqual => "`>=`",
            TokenKind::Less => "`<`",
            TokenKind::LessEqual => "`<=`",
            TokenKind::LParen => "`(`",
            TokenKind::RParen => "`)`",
            TokenKind::LBracket => "`[`",
            TokenKind::RBracket => "`]`",
            TokenKind::Dot => "`.`",
            TokenKind::Comma => "`,`",
            TokenKind::Const => "`const`",
            TokenKind::Global => "`global`",
            TokenKind::Array => "`array`",
            TokenKind::Equal => "`=`",
            TokenKind::Ident => "identifier",
            TokenKind::If => "`if`",
            TokenKind::Then => "`then`",
            TokenKind::Elseif => "`elseif`",
            TokenKind::Else => "`else`",
            TokenKind::Endif => "`endif`",
            TokenKind::Switch => "`switch`",
            TokenKind::Case => "`case`",
            TokenKind::Default => "`default`",
            TokenKind::Endswitch => "`endswitch`",
            TokenKind::For => "`for`",
            TokenKind::To => "`to`",
            TokenKind::Step => "`step`",
            TokenKind::Next => "`next`",
            TokenKind::Do => "`do`",
            TokenKind::Until => "`until`",
            TokenKind::While => "`while`",
            TokenKind::Endwhile => "`endwhile`",
            TokenKind::Function => "`function`",
            TokenKind::Return => "`return`",
            TokenKind::Endfunction => "`endfunction`",
            TokenKind::Procedure => "`procedure`",
            TokenKind::Endprocedure => "`endprocedure`",
            TokenKind::Import => "`import`",
            TokenKind::String => "string",
            TokenKind::Char => "char",
            TokenKind::Number => "number",
            TokenKind::True => "`true`",
            TokenKind::False => "`false`",
            TokenKind::Newline => "newline",
            TokenKind::Whitespace => "whitespace",
            TokenKind::Comment => "comment",
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(src: &str, expected: TokenKind) {
        let mut lex = TokenKind::lexer(src);

        assert_eq!(lex.next().unwrap().unwrap(), expected);
        assert_eq!(lex.slice(), src);
    }

    #[test]
    fn lex_plus() {
        check("+", TokenKind::Plus);
    }

    #[test]
    fn lex_minus() {
        check("-", TokenKind::Minus);
    }

    #[test]
    fn lex_star() {
        check("*", TokenKind::Star);
    }

    #[test]
    fn lex_slash() {
        check("/", TokenKind::Slash);
    }

    #[test]
    fn lex_caret() {
        check("^", TokenKind::Caret);
    }

    #[test]
    fn lex_colon() {
        check(":", TokenKind::Colon);
    }

    #[test]
    fn lex_mod() {
        check("MOD", TokenKind::Mod);
    }

    #[test]
    fn lex_div() {
        check("DIV", TokenKind::Div);
    }

    #[test]
    fn lex_and() {
        check("AND", TokenKind::And);
    }

    #[test]
    fn lex_or() {
        check("OR", TokenKind::Or);
    }

    #[test]
    fn lex_not() {
        check("NOT", TokenKind::Not);
    }

    #[test]
    fn lex_const() {
        check("const", TokenKind::Const);
    }

    #[test]
    fn lex_global() {
        check("global", TokenKind::Global);
    }

    #[test]
    fn lex_array() {
        check("array", TokenKind::Array);
    }

    #[test]
    fn lex_equal() {
        check("=", TokenKind::Equal);
    }

    #[test]
    fn lex_ident() {
        check("an_ident123", TokenKind::Ident);
    }

    #[test]
    fn lex_equal_equal() {
        check("==", TokenKind::EqualEqual);
    }

    #[test]
    fn lex_less() {
        check("<", TokenKind::Less);
    }

    #[test]
    fn lex_less_equal() {
        check("<=", TokenKind::LessEqual);
    }

    #[test]
    fn lex_greater() {
        check(">", TokenKind::Greater);
    }

    #[test]
    fn lex_greater_equal() {
        check(">=", TokenKind::GreaterEqual);
    }

    #[test]
    fn lex_bang_equal() {
        check("!=", TokenKind::BangEqual);
    }

    #[test]
    fn lex_parens() {
        check("(", TokenKind::LParen);
        check(")", TokenKind::RParen);
    }

    #[test]
    fn lex_brackets() {
        check("[", TokenKind::LBracket);
        check("]", TokenKind::RBracket);
    }

    #[test]
    fn lex_dot() {
        check(".", TokenKind::Dot);
    }

    #[test]
    fn lex_comma() {
        check(",", TokenKind::Comma);
    }

    #[test]
    fn lex_if() {
        check("if", TokenKind::If);
    }

    #[test]
    fn lex_then() {
        check("then", TokenKind::Then);
    }

    #[test]
    fn lex_elseif() {
        check("elseif", TokenKind::Elseif);
    }

    #[test]
    fn lex_else() {
        check("else", TokenKind::Else);
    }

    #[test]
    fn lex_endif() {
        check("endif", TokenKind::Endif);
    }

    #[test]
    fn lex_switch() {
        check("switch", TokenKind::Switch);
    }

    #[test]
    fn lex_case() {
        check("case", TokenKind::Case);
    }

    #[test]
    fn lex_default() {
        check("default", TokenKind::Default);
    }

    #[test]
    fn lex_endswitch() {
        check("endswitch", TokenKind::Endswitch);
    }

    #[test]
    fn lex_for() {
        check("for", TokenKind::For);
    }

    #[test]
    fn lex_to() {
        check("to", TokenKind::To);
    }

    #[test]
    fn lex_step() {
        check("step", TokenKind::Step);
    }

    #[test]
    fn lex_next() {
        check("next", TokenKind::Next);
    }

    #[test]
    fn lex_do() {
        check("do", TokenKind::Do);
    }

    #[test]
    fn lex_until() {
        check("until", TokenKind::Until);
    }

    #[test]
    fn lex_while() {
        check("while", TokenKind::While);
    }

    #[test]
    fn lex_endwhile() {
        check("endwhile", TokenKind::Endwhile);
    }

    #[test]
    fn lex_function() {
        check("function", TokenKind::Function);
    }

    #[test]
    fn lex_return() {
        check("return", TokenKind::Return);
    }

    #[test]
    fn lex_endfunction() {
        check("endfunction", TokenKind::Endfunction);
    }

    #[test]
    fn lex_procedure() {
        check("procedure", TokenKind::Procedure);
    }

    #[test]
    fn lex_endprocedure() {
        check("endprocedure", TokenKind::Endprocedure);
    }

    #[test]
    fn lex_whole_number() {
        check("42", TokenKind::Number);
    }

    #[test]
    fn lex_decimal_number() {
        check("3.14", TokenKind::Number);
    }

    #[test]
    fn lex_bool_literals() {
        check("true", TokenKind::True);
        check("false", TokenKind::False);
    }

    #[test]
    fn lex_string_literal() {
        check(r#""A string""#, TokenKind::String);
    }

    #[test]
    fn lex_escaped_string_literal() {
        check(r#""An \"escaped\" string""#, TokenKind::String);
    }

    #[test]
    fn lex_char_literal() {
        check("'c'", TokenKind::Char);
    }

    #[test]
    fn lex_escaped_char_literal() {
        check(r"'\''", TokenKind::Char);
    }

    #[test]
    fn lex_comment() {
        check("// A comment", TokenKind::Comment);
    }

    #[test]
    fn lex_whitespace() {
        check("  \t", TokenKind::Whitespace);
    }

    #[test]
    fn lex_newline() {
        check("\n", TokenKind::Newline);
        check("\r\n", TokenKind::Newline);
    }

    #[test]
    fn lex_import() {
        check("import", TokenKind::Import)
    }
}
