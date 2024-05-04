#![allow(unused, dead_code)]

use std::{collections::HashMap, error::{self, Error}, io::Write, process::exit};

use parser::Parser;

use crate::{interpreter::Interpreter, lexer::lex, parser::parse};

mod interpreter;
mod lexer;
mod parser;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        repl();
    } else {
        let tokens = lexer::lex(match std::fs::read_to_string(&args[1]) {
            Err(e) => error(1, &format!("'{}' {e}", &args[1])),
            Ok(ref v) => v,
        });
        let mut interpreter = Interpreter::new();
        interpreter.error(true);
        let mut parser = Parser::new(tokens);
        parser.error(true);
        interpreter.exec(match parser.parse() {
            Ok(ref v) => v,
            Err(ref errors) => {
                let mut error_msg = String::new();
                for err in errors {
                    error_msg += "\n";
                    error_msg += &format_err(err);
                }
                println!("{error_msg}");
                exit(1);
            },
        });
    }
}

pub fn repl() {
    let mut buf = String::new();
    let mut stdout = std::io::stdout();
    let stdin = std::io::stdin();
    let mut inter = Interpreter::new();
    println!("Type 'exit;' to exit");
    loop {
        buf.clear();
        print!("\x1b[38;2;0;0;255m[REPL]:|>\x1b[0m ");
        stdout.flush();
        match stdin.read_line(&mut buf) {
            Ok(_) => {
                if buf.trim() == "exit;" {
                    break;
                }
                match parse(lex(&buf)) {
                    Ok(ref v) => println!("{}", match inter.exec(v) {
                        Ok(v) => v.dbg_str(),
                        Err(err) => { println!("{}", &format_err(&err)); continue },
                    }),
                    Err(errors) => {
                        for err in errors {
                            println!("{}", format_err(&err));
                        }
                    },
                }
            }
            Err(e) => print_err(&e.to_string()),
        }
    }
}

pub fn error(code: i32, msg: &str) -> ! {
    print_err(msg);
    std::process::exit(code);
}

pub fn print_err(msg: &str) {
    println!("\n{}", format_err(msg));
}

pub fn format_err(msg: &str) -> String {
    format!("\x1b[48;2;255;0;0m[ERROR]:\x1b[0m\x1b[38;2;255;0;0m {msg}\x1b[0m")
}
