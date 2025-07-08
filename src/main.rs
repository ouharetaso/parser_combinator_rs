mod json;
use json::parse_json;
use std::io::{stdin, Read};

fn main() -> std::result::Result<(), Box<dyn std::error::Error>>{
    let mut buf = Vec::new();
    stdin().read_to_end(&mut buf)?;

    let string = String::from_utf8(buf)?;

    match parse_json(&string) {
        Some(value) => {
            print!("{:?}", value)
        }
        None => {
            eprintln!("parse failed")
        }
    }

    Ok(())
}
