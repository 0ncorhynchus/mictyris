pub mod engine;
pub mod lexer;
pub mod parser;
pub mod pass;

use engine::Value::*;
use std::io::{self, Write};

fn main() -> io::Result<()> {
    let mut engine = engine::Engine::new();
    engine.register("pi", Number(std::f64::consts::PI));
    engine.register_proc("print", |values| {
        let strings: Vec<_> = values.iter().map(|v| v.to_string()).collect();
        let strings = strings.join(" ");
        println!("{}", strings);
        Unspecified
    });

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
        let value = match engine.eval(&ast) {
            Some(value) => value,
            None => continue,
        };
        println!("{}", value);
    }
    Ok(())
}
