#![cfg(test)]
extern crate test_generator;

use std::fs;
use std::path::Path;
use std::rc::Rc;
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use rustyline::Editor;
use test_generator::test_resources;
use crate::analysis::analyze_stmt;
use crate::StackFrame;

use crate::syntax::grammar::td5;

#[test_resources("examples/*.td5")]
fn check_parse(file: &str)
{
    let src = fs::read_to_string(Path::new(file)).expect("Unable to read file");

    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();
    let mut editor = Editor::<()>::new();

    match td5::erc_statement(src.as_str())
    {
        Ok(r) => {
            let frame = Rc::new(StackFrame::empty());
            if let Some(e) = analyze_stmt(&r, &frame).err() {
                let file = SimpleFile::new("<repl>", src);
                term::emit(&mut writer.lock(), &config, &file, &e).unwrap();
                panic!();
            }
            println!("{:?}\n{}", r, r);
        },
        Err(e) =>
            {
                println!("{:?}", e);
                panic!()
            }
    }
}