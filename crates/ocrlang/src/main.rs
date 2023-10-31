use interpreter::Interpreter;

use color_eyre::Result;
use rustyline::{error::ReadlineError, DefaultEditor};

fn main() -> Result<()> {
    let mut interpreter = Interpreter::default();

    if let Some(file) = std::env::args().nth(1) {
        return interpreter
            .run(&std::fs::read_to_string(file)?)
            .map_err(|e| e.into());
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
