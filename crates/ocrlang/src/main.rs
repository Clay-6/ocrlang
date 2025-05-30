use std::{fs, path::PathBuf};

use interpreter::{InterpretError, Interpreter};

use clap::Parser;
use color_eyre::Result;
use line_index::{LineIndex, TextRange};
use rustyline::{DefaultEditor, error::ReadlineError};

fn main() -> Result<()> {
    color_eyre::install()?;
    let args = Args::parse();
    let mut interpreter = Interpreter::default();

    if let Some(path) = args.file {
        if !path.exists() {
            eprintln!("Error: file `{}` does not exist", path.display());
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
    } else if let Some(cmd) = args.exec {
        let res = interpreter
            .run(&cmd)
            .map_err(|e| {
                interpret_err(e, LineIndex::new(&cmd));
                std::process::exit(2)
            })
            .expect("If there was an error, we already exited");
        match res {
            interpreter::Value::Unit => {} // Nothing to print
            val => println!("{}", val),
        }
        return Ok(());
    }

    let mut rl = DefaultEditor::new()?;
    loop {
        let read_line = rl.readline("> ");
        match read_line {
            Ok(line) => {
                if line == "quit()" || line == "quit" {
                    break;
                }

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
        InterpretError::LexError { text } => {
            if let Some(linecol) = line_index.try_line_col(range.start()) {
                eprintln!(
                    "Invalid token '{}' on line {}, column {}",
                    text,
                    linecol.line + 1,
                    linecol.col + 1
                );
            } else {
                eprintln!("Invalid token '{}' in an imported file", text);
            }
        }
        InterpretError::ParseErrors { errors } => {
            let mut last_linecol = None;
            for err in errors {
                let linecol =
                    line_index.try_line_col(err.text_range().unwrap().start());
                if match (last_linecol, linecol) {
                    (Some(ll), Some(lc)) => ll != lc,
                    (None, Some(_)) => true,
                    _ => false,
                } {
                    // Unwraps are fine since if we got here then `linecol` is *definitely* `Some`
                    eprintln!(
                        "Error on line {} column {}: {}",
                        linecol.unwrap().line + 1,
                        linecol.unwrap().col + 1,
                        err.context()
                    );
                } else if linecol.is_none() {
                    eprintln!("Error in an imported file: {}", err.context())
                }
                last_linecol = linecol
            }
        }
        _ => {
            let linecol = line_index.line_col(range.start());
            eprintln!(
                "Error at line {}, column {}: {}",
                linecol.line + 1,
                linecol.col + 1,
                e
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
    /// Execute a single line of Exam Reference Language code.
    ///
    /// Will be ignored if a file is provided
    #[clap(short = 'c', long = "exec")]
    exec: Option<String>,
}
