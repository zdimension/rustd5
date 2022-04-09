mod tests;
mod syntax;
mod analysis;

use std::fs;
use std::path::Path;
use std::rc::Rc;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use crate::analysis::StackFrame;


fn main() -> anyhow::Result<()> {
    let src = fs::read_to_string(Path::new("examples/calc_rpn.td5")).expect("Unable to read file");

    match syntax::grammar::td5::erc_statement(src.as_str())
    {
        Ok(r) => println!("{:?}\n{}", r, r),
        Err(e) =>
            {
                println!("{:?}", e);
                panic!()
            }
    }

    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();
    let mut editor = Editor::<()>::new();

    let frame = Rc::new(StackFrame::empty());
    loop {
        let line = match editor.readline("> ") {
            Ok(line) => line,
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => return Ok(()),
            Err(error) => return Err(error.into()),
        };

        match syntax::grammar::td5::erc_expr(&line) {
            Ok(number) => {
                match analysis::analyze_expr(&number, &frame) {
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