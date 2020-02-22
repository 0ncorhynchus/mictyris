pub mod lexer;

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

        let lexer = lexer::Lexer::new(&buffer);
        let tokens: Vec<_> = lexer.collect();
        println!("{:?}", tokens);
    }
    Ok(())
}
