mod event;
mod grammar;
mod parser;
mod sink;
mod source;

pub use crate::parser::parse_error::ParseError;
use crate::{parser::Parser, sink::Sink, source::Source};

use lexer::Lexer;
use rowan::GreenNode;
use syntax::SyntaxNode;

#[must_use]
pub fn parse(input: &str) -> Parse {
    let tokens = Lexer::new(input).collect::<Vec<_>>();
    let source = Source::new(&tokens);
    let parser = Parser::new(source);
    let events = parser.parse();
    let sink = Sink::new(&tokens, events);

    sink.finish()
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

    pub fn errors(&self) -> &[ParseError] {
        &self.errors
    }
}

#[cfg(test)]
fn check(input: &str, expected_tree: expect_test::Expect) {
    let tree = parse(input).debug_tree();

    expected_tree.assert_eq(&tree);
}
