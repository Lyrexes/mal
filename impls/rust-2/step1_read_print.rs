use printer::pr_str;
use reader::read_str;
use rustyline::error::ReadlineError;
use types::MalType;
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
                Err(err) => err
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

fn rep(string: String) -> Result<String, String> {
    print(eval(read(string)?)?)
}

fn read(string: String) -> Result<MalType, String>  {
    read_str(string)
}

fn eval(ast: MalType) -> Result<MalType, String> {
    Ok(ast)
}

fn print(ast: MalType) -> Result<String, String> {
    pr_str(&ast, true)
}