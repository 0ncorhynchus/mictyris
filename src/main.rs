pub mod engine;
pub mod lexer;
pub mod parser;
pub mod pass;

use engine::Value::*;
use std::io::{self, Write};

fn main() -> io::Result<()> {
    let mut engine = engine::Engine::new();
    engine.register("pi", Number(std::f64::consts::PI));

    loop {
        print!("mictyris> ");
        io::stdout().flush()?;

        let mut buffer = String::new();
        if io::stdin().read_line(&mut buffer)? == 0 {
            // EOF
            println!();
            break;
        }

        match buffer.trim() {
            "" => continue,
            "quit" => break,
            _ => (),
        }

        let mut parser = parser::Parser::new(&buffer);
        let expr = match parser.parse() {
            Some(expr) => expr,
            None => continue,
        };
        let ast = match pass::pass(&expr) {
            Some(ast) => ast,
            None => continue,
        };
        engine.eval_and_print(&ast);
    }
    Ok(())
}
