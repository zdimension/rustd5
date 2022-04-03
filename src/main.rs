mod tests;
mod syntax;
mod analysis;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use rustyline::error::ReadlineError;
use rustyline::Editor;


fn main() -> anyhow::Result<()> {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();
    let mut editor = Editor::<()>::new();

    loop {
        let line = match editor.readline("> ") {
            Ok(line) => line,
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => return Ok(()),
            Err(error) => return Err(error.into()),
        };

        match syntax::grammar::td5::erc_expr(&line) {
            Ok(number) => {
                match analysis::analysis(number.clone()) {
                    Ok(_) => println!("{}", number),
                    Err(error) => {
                        let file = SimpleFile::new("<repl>", line);
                        term::emit(&mut writer.lock(), &config, &file, &error)?;
                    }
                }
            },
            Err(error) => {
                let file = SimpleFile::new("<repl>", line);

                let start = error.location.offset;
                let diagnostic = Diagnostic::error()
                    .with_message("parse error")
                    .with_labels(vec![
                        Label::primary((), start..start).with_message("parse error")
                    ])
                    .with_notes(vec![format!("expected: {}", error.expected)]);

                term::emit(&mut writer.lock(), &config, &file, &diagnostic)?;
            }
        }
    }
}