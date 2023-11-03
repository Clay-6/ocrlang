mod event;
mod grammar;
mod parser;
mod sink;
mod source;

use std::ops::Range;

pub use crate::parser::parse_error::ParseError;
use crate::{parser::Parser, sink::Sink, source::Source};

use lexer::Lexer;
use rowan::GreenNode;
use syntax::SyntaxNode;

pub fn parse(input: &str) -> Result<Parse, LexError> {
    let tokens = Lexer::new(input)
        .collect::<Result<Vec<_>, _>>()
        .map_err(|(text, range)| LexError { text, range })?;
    let source = Source::new(&tokens);
    let parser = Parser::new(source);
    let events = parser.parse();
    let sink = Sink::new(&tokens, events);

    Ok(sink.finish())
}

#[derive(Debug, Clone, thiserror::Error)]
#[error("Error at {text} ({range:?})")]
pub struct LexError<'i> {
    pub text: &'i str,
    pub range: Range<usize>,
}

pub struct Parse {
    green_node: GreenNode,
    errors: Vec<ParseError>,
}

impl Parse {
    #[must_use]
    pub fn debug_tree(&self) -> String {
        let mut s = String::new();

        let tree = format!("{:#?}", self.syntax());

        // Cut off trailing newline from formatting
        s.push_str(&tree[0..tree.len() - 1]);

        for error in &self.errors {
            s.push_str(&format!("\n{error}"));
        }

        s
    }

    #[must_use]
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }

    #[must_use]
    pub fn errors(&self) -> &[ParseError] {
        &self.errors
    }
}

impl core::fmt::Debug for Parse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.debug_tree())
    }
}

#[cfg(test)]
fn check(input: &str, expected_tree: expect_test::Expect) {
    let tree = parse(input).unwrap().debug_tree();

    expected_tree.assert_eq(&tree);
}
