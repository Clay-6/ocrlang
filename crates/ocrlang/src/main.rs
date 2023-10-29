use std::io::{self, Write};

use interpreter::Interpreter;

fn main() -> io::Result<()> {
    let mut input = String::new();
    let stdout = io::stdout();
    let stdin = io::stdin();
    let mut stderr = io::stderr();
    let mut interpreter = Interpreter::new(stdin, stdout);

    if let Some(file) = std::env::args().nth(1) {
        if let Err(e) = interpreter.run(&std::fs::read_to_string(file)?) {
            writeln!(stderr, "{e}").unwrap();
            std::process::exit(65);
        }
        return Ok(());
    }

    loop {
        write!(stderr, "> ")?;
        stderr.flush()?;
        io::stdin().read_line(&mut input)?;

        if input.is_empty() {
            break Ok(());
        }

        if let Err(e) = interpreter.run(&input) {
            writeln!(stderr, "{e}").unwrap();
        }

        input.clear();
    }
}
