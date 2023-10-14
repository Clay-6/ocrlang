use std::io::{self, Write};

use interpreter::Interpreter;

fn main() -> io::Result<()> {
    let mut input = String::new();
    let stdout = io::stdout();
    let stdin = io::stdin();
    let mut stderr = io::stderr();
    let mut interpreter = Interpreter::new(stdout);

    loop {
        write!(stderr, "> ")?;
        stderr.flush()?;
        stdin.read_line(&mut input)?;

        interpreter.run(&input).unwrap();

        input.clear();
    }
}
