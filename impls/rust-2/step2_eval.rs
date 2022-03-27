use fnv::FnvHashMap;
use std::{rc::Rc};
use printer::pr_str;
use reader::read_str;
use rustyline::error::ReadlineError;
use types::{MalType, MalType::Number, MalType::Symbol, MalType::Vector, 
            MalType::List, MalType::Builtin, MalType::HashMap, MalRet};
mod reader;
mod types;
mod printer;
mod env;
type EnvTable = FnvHashMap<String, MalType>;


fn main() {

    let mut repl_env = EnvTable::default();

    repl_env.insert("+".to_string(),Builtin(|args| { 
        match (&args[0],  &args[1]) {
            (Number(lhs), Number(rhs)) => Ok(Number(lhs + rhs)),
            _ => Err("EOF: invalid add".to_string())
        }
    }));
    
    repl_env.insert("-".to_string(), Builtin(|args| { 
        match (&args[0], &args[1]) {
            (Number(lhs), MalType::Number(rhs)) => Ok(MalType::Number(lhs - rhs)),
            _ => Err("EOF invalid subtraction".to_string())
        }
    }));

    repl_env.insert("*".to_string(), Builtin(|args| { 
        match (&args[0], &args[1]) {
            (Number(lhs), MalType::Number(rhs)) => Ok(MalType::Number(lhs * rhs)),
            _ => Err("Eof invalid mutltiplication".to_string())
        }
    }));

    repl_env.insert("/".to_string(), Builtin(|args| { 
        match (&args[0], &args[1]) {
            (Number(lhs), MalType::Number(rhs)) =>
                 Ok(Number(lhs.checked_div(*rhs)
                 .ok_or("invalid division!".to_string())?)),
            _ => Err("Eof invalid division".to_string())
        }
    }));


    loop {
        match input("user> ") {
            Ok(line) => 
            println!("{}", match rep(line, &repl_env) {
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

fn rep(string: String, env: &EnvTable) -> Result<String, String> {
    print(eval(&read(string)?, env)?)
}

fn read(string: String) -> MalRet  {
    read_str(string)
}

fn eval(ast: &MalType, env: &EnvTable) -> MalRet {
    match ast {
        List(list) => {
            if list.is_empty() { return Ok(ast.clone()) }
            match eval_ast(ast, env)? {
                List(eval_list) => return Ok(eval_list[0].apply(&eval_list[1..])?),
                _ => return Err("EOF: expected list!".to_owned()),
            }
        }
        _ => eval_ast(ast, env).clone()
    }
}

fn eval_ast(ast: &MalType, env: &EnvTable) -> MalRet {
    match ast {
        Symbol(sym) => {
            Ok(env.get(&(**sym))
             .ok_or("EOF: symbol not found!")?
             .clone())
        }
        List(list) | Vector(list) => {
            let mut new_seq = Vec::<MalType>::with_capacity(list.len());
            for l in (*list).iter() {
                new_seq.push(eval(l, env)?);
            }
            if let List(_) = ast {
                Ok(list!(new_seq))
            } else {
                Ok(vector!(new_seq))
            }
        }
        HashMap(map) => {
            let mut new_map = std::collections::HashMap::with_capacity_and_hasher(
            map.len(),
        fnv::FnvBuildHasher::default()
            );
            for (k, v) in (*map).iter() {
                new_map.insert(k.to_string(), eval(v, env)?);
            }
                Ok(map!(new_map))
        }
        _ => Ok(ast.clone())
    }
}

fn print(ast: MalType) -> Result<String, String> {
    pr_str(&ast, true)
}
