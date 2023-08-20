mod event;
mod grammar;
mod parser;
mod sink;
mod source;

use lexer::Lexer;
use parser::{ParseError, Parser};
use rowan::GreenNode;
use sink::Sink;
use source::Source;

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
