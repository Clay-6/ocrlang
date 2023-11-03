use std::{fs, path::PathBuf};

use interpreter::Interpreter;

use color_eyre::Result;
use rustyline::{error::ReadlineError, DefaultEditor};

fn main() -> Result<()> {
    color_eyre::install()?;
    let args = argh::from_env::<Args>();
    let mut interpreter = Interpreter::default();

    if let Some(path) = args.file {
        return interpreter
            .run(&fs::read_to_string(path)?)
            .map(|_| ())
            .map_err(Into::into);
    }

    let mut rl = DefaultEditor::new()?;
    loop {
        let read_line = rl.readline("> ");
        match read_line {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;
                match interpreter.run(&line) {
                    Ok(interpreter::Value::Unit) => {}
                    Ok(v) => println!("{v}"),
                    Err(e) => eprintln!("{e}"),
                }
            }
            Err(ReadlineError::Eof | ReadlineError::Interrupted) => break,
            Err(e) => return Err(e.into()),
        }
    }

    Ok(())
}

/// Execute OCR Exam Reference Language code from a file
/// or in a REPL
#[derive(argh::FromArgs)]
struct Args {
    /// A file to execute
    #[argh(positional)]
    file: Option<PathBuf>,
}
