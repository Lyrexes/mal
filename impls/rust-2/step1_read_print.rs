use rustyline::error::ReadlineError;
mod reader;

fn main() {
    loop {
        match input("user> ") {
            Ok(line) => println!("{}", rep(line)),
            _ => return,
        }
    }
}

fn input(prompt: &str) -> Result<String, ReadlineError> {
    let mut rl = rustyline::Editor::<()>::new();
    let input = rl.readline(prompt);
    input
}

fn rep(string: String) -> String {
    print(eval(read(string)))
}

fn read(string: String) -> String  {
    string
}

fn eval(string: String) -> String  {
    string
}

fn print(string: String) -> String {
    string
}