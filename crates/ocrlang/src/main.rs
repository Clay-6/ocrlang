use std::io::{self, Write};

use interpreter::Interpreter;

fn main() -> io::Result<()> {
    let mut input = String::new();
    let mut stdout = io::stdout();
    let stdin = io::stdin();
    let mut interpreter = Interpreter::new();

    loop {
        write!(stdout, "> ")?;
        stdout.flush()?;
        stdin.read_line(&mut input)?;

        interpreter.run(&input).unwrap();

        dbg!(&interpreter);

        input.clear();
    }
}
