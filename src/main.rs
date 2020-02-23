pub mod lexer;
pub mod parser;

use std::io::{self, Write};

fn main() -> io::Result<()> {
    loop {
        print!("mictyris> ");
        io::stdout().flush()?;

        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer)?;

        match buffer.trim() {
            "" => continue,
            "quit" => break,
            _ => (),
        }

        let mut parser = parser::Parser::new(&buffer);
        match parser.parse() {
            Some(expr) => println!("{:?}", expr),
            None => (),
        }
    }
    Ok(())
}
