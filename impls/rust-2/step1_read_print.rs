use printer::pr_str;
use reader::read_str;
use rustyline::error::ReadlineError;
use types::{MalType, MalError};
mod reader;
mod types;
mod printer;
mod env;

fn main() {
    loop {
        match input("user> ") {
            Ok(line) => 
            println!("{}", match rep(line) {
                Ok(str) => str,
                Err(err) => match err {
                    MalError::MalVal(v) => pr_str(&v, true),
                    MalError::Message(msg) => format!("EOF: {}", msg),
                }
            }),
            _ => return,
        }
    }
}

fn input(prompt: &str) -> Result<String, ReadlineError> {
    let mut rl = rustyline::Editor::<()>::new();
    let input = rl.readline(prompt);
    input
}

fn rep(string: String) -> Result<String, MalError> {
    Ok(print(eval(read(string)?)?))
}

fn read(string: String) -> Result<MalType, MalError>  {
    read_str(string)
}

fn eval(ast: MalType) -> Result<MalType, MalError> {
    Ok(ast)
}

fn print(ast: MalType) -> String {
    pr_str(&ast, true)
}