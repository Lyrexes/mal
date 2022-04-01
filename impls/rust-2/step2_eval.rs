use fnv::FnvHashMap;
use std::{rc::Rc};
use printer::pr_str;
use reader::read_str;
use rustyline::error::ReadlineError;
use types::{MalType, MalType::Number, MalType::Symbol, MalType::Vector, 
            MalType::List, MalType::Builtin, MalType::HashMap, MalRet, MalError};
mod reader;
mod types;
mod printer;
mod env;
type EnvTable = FnvHashMap<String, MalType>;


fn main() {

    let mut repl_env = EnvTable::default();

    repl_env.insert("+".to_string(), builtin!(|args| { 
        match (&args[0],  &args[1]) {
            (Number(lhs), Number(rhs)) => Ok(Number(lhs + rhs)),
            _ => error_msg!("invalid add".to_string())
        }
    }));
    
    repl_env.insert("-".to_string(), builtin!(|args| { 
        match (&args[0], &args[1]) {
            (Number(lhs), MalType::Number(rhs)) => Ok(MalType::Number(lhs - rhs)),
            _ => error_msg!("EOF invalid subtraction".to_string())
        }
    }));

    repl_env.insert("*".to_string(), builtin!(|args| { 
        match (&args[0], &args[1]) {
            (Number(lhs), MalType::Number(rhs)) => Ok(MalType::Number(lhs * rhs)),
            _ => error_msg!("Eof invalid mutltiplication".to_string())
        }
    }));

    repl_env.insert("/".to_string(), builtin!(|args| { 
        match (&args[0], &args[1]) {
            (Number(lhs), MalType::Number(rhs)) =>
                 Ok(Number(lhs.checked_div(*rhs)
                 .ok_or(MalError::Message("invalid division!".to_string()))?)),
            _ => error_msg!("Eof invalid division".to_string())
        }
    }));


    loop {
        match input("user> ") {
            Ok(line) => 
            println!("{}", match rep(line, &repl_env) {
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

fn rep(string: String, env: &EnvTable) -> Result<String, MalError> {
    Ok(print(eval(&read(string)?, env)?))
}

fn read(string: String) -> MalRet  {
    read_str(string)
}

fn eval(ast: &MalType, env: &EnvTable) -> MalRet {
    match ast {
        List(list,_) => {
            if list.is_empty() { return Ok(ast.clone()) }
            match eval_ast(ast, env)? {
                List(eval_list,_) => return Ok(eval_list[0].apply(&eval_list[1..])?),
                _ => return error_msg!("expected list!"),
            }
        }
        _ => eval_ast(ast, env).clone()
    }
}

fn eval_ast(ast: &MalType, env: &EnvTable) -> MalRet {
    match ast {
        Symbol(sym) => {
            Ok(env.get(&(**sym))
             .ok_or(MalError::Message("symbol not found!".to_string()))?
             .clone())
        }
        List(list,_) | Vector(list,_) => {
            let mut new_seq = Vec::<MalType>::with_capacity(list.len());
            for l in (*list).iter() {
                new_seq.push(eval(l, env)?);
            }
            if let List(_,_) = ast {
                Ok(list!(new_seq))
            } else {
                Ok(vector!(new_seq))
            }
        }
        HashMap(map,_) => {
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

fn print(ast: MalType) -> String {
    pr_str(&ast, true)
}
