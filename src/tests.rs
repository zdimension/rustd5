#![cfg(test)]
extern crate test_generator;

use std::fs;
use std::path::Path;
use test_generator::test_resources;

use crate::pegparser::td5;

#[test_resources("examples/*.td5")]
fn check_parse(file: &str)
{
    let src = fs::read_to_string(Path::new(file)).expect("Unable to read file");

    match td5::statement(src.as_str())
    {
        Ok(r) => println!("{:?}\n{}", r, r),
        Err(e) =>
            {
                println!("{:?}", e);
                panic!()
            }
    }
}