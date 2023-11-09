use std::{fs, path::PathBuf};

use interpreter::{InterpretError, Interpreter};

use clap::Parser;
use color_eyre::Result;
use line_index::{LineIndex, TextRange, TextSize};
use rustyline::{error::ReadlineError, DefaultEditor};

fn main() -> Result<()> {
    color_eyre::install()?;
    let args = Args::parse();
    let mut interpreter = Interpreter::default();

    if let Some(path) = args.file {
        if !path.exists() {
            eprintln!("Error: file {} does not exist", path.display());
            std::process::exit(1);
        }
        let text = fs::read_to_string(path)?;
        let line_index = LineIndex::new(&text);
        return match interpreter.run(&text).map(|_| ()) {
            Ok(()) => Ok(()),

            Err(e) => {
                interpret_err(e, line_index);
                std::process::exit(2);
            }
        };
    }

    let mut rl = DefaultEditor::new()?;
    loop {
        let read_line = rl.readline("> ");
        match read_line {
            Ok(line) => {
                let lineindex = LineIndex::new(&line);
                rl.add_history_entry(line.as_str())?;
                match interpreter.run(&line) {
                    Ok(interpreter::Value::Unit) => {}
                    Ok(v) => println!("{v}"),
                    Err(e) => interpret_err(e, lineindex),
                }
            }
            Err(ReadlineError::Eof | ReadlineError::Interrupted) => break,
            Err(e) => return Err(e.into()),
        }
    }

    Ok(())
}

fn interpret_err(
    (range, e): (TextRange, InterpretError),
    line_index: LineIndex,
) {
    match e {
        InterpretError::LexError { text, range } => {
            let linecol =
                line_index.line_col(TextSize::new(range.start as u32));
            eprintln!(
                "Invalid token '{}' on line {}, column {}",
                text,
                linecol.line + 1,
                linecol.col + 1
            );
        }
        InterpretError::ParseErrors { errors } => {
            let mut last_linecol = None;
            for err in errors {
                let linecol =
                    line_index.line_col(err.text_range().unwrap().start());
                if !last_linecol.is_some_and(|lc| linecol == lc) {
                    eprintln!(
                        "Error on line {} column {}: {}",
                        linecol.line,
                        linecol.col,
                        err.context()
                    );
                }
                last_linecol = Some(linecol)
            }
        }
        _ => {
            let linecol = line_index.line_col(range.start());
            eprintln!(
                "Error at line {}, column {}: {}",
                linecol.line, linecol.col, e
            )
        }
    };
}

/// Execute OCR Exam Reference Language code from a file
/// or in a REPL
#[derive(Debug, Clone, Parser)]
pub struct Args {
    /// A file to execute
    file: Option<PathBuf>,
}
