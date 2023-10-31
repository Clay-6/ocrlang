use interpreter::Interpreter;

use color_eyre::Result;
use rustyline::{error::ReadlineError, DefaultEditor};

fn main() -> Result<()> {
    let mut interpreter = Interpreter::new(std::io::stdin(), std::io::stdout());

    if let Some(file) = std::env::args().nth(1) {
        if let Err(e) = interpreter.run(&std::fs::read_to_string(file)?) {
            eprintln!("{e}");
            std::process::exit(65);
        }
        return Ok(());
    }

    let mut rl = DefaultEditor::new()?;
    loop {
        let read_line = rl.readline("> ");
        match read_line {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;
                if let Err(e) = interpreter.run(&line) {
                    eprintln!("{e}");
                };
            }
            Err(ReadlineError::Eof | ReadlineError::Interrupted) => break,
            Err(e) => return Err(e.into()),
        }
    }

    Ok(())
}
