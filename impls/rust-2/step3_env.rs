use env::{Env, EnvInterface};
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


fn main() {
    let mut repl_env = Env::new_env(None);

    repl_env.set("+".to_string(),Builtin(|args| { 
        match (&args[0],  &args[1]) {
            (Number(lhs), Number(rhs)) => Ok(Number(lhs + rhs)),
            _ => Err("EOF: invalid add".to_string())
        }
    }));
    
    repl_env.set("-".to_string(), Builtin(|args| { 
        match (&args[0], &args[1]) {
            (Number(lhs), MalType::Number(rhs)) => Ok(MalType::Number(lhs - rhs)),
            _ => Err("EOF invalid subtraction".to_string())
        }
    }));

    repl_env.set("*".to_string(), Builtin(|args| { 
        match (&args[0], &args[1]) {
            (Number(lhs), MalType::Number(rhs)) => Ok(MalType::Number(lhs * rhs)),
            _ => Err("Eof invalid mutltiplication".to_string())
        }
    }));

    repl_env.set("/".to_string(), Builtin(|args| { 
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
            println!("{}", match rep(line, &mut repl_env) {
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

fn rep(string: String, env: &mut Env) -> Result<String, String> {
    print(eval(&read(string)?,env)?)
}

fn read(string: String) -> MalRet  {
    read_str(string)
}

fn eval(ast: &MalType, env: &mut Env) -> MalRet {
    match ast {
        List(list) => {
            if list.is_empty() { return Ok(ast.clone()) }
            let first = &list[0];
            match first {
                Symbol(s) if **s == "def!" => {
                    apply_def(&list[1..], env)
                },
                Symbol(s) if **s == "let*" => {
                    apply_let(&list[1..], env)
                }
                _ => if let List(eval_list) = eval_ast(ast, env)? {
                    return Ok(eval_list[0].apply(&eval_list[1..])?)
                } else {
                    return Err("EOF: expected list!".to_owned())
                }
            }
        }
        _ => eval_ast(ast, env).clone()
    }
}

fn eval_ast(ast: &MalType, env: &mut Env) -> MalRet {
    match ast {
        Symbol(sym) => {
            Ok(env.get(&(**sym))
             .ok_or(format!("EOF: {} not found!", sym.to_string()))?
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

fn apply_def(args: &[MalType], env: &mut Env) -> MalRet {
    validate_args(args, 2, "def!");
    if let Symbol(key) = &args[0] {
        let eval_val = eval(&args[1], env)?;
        env.set((**key).clone(), eval_val.clone());
        return Ok(eval_val);
    }
    Err(format!("EOF: binding var must be a symbol! got: {}", pr_str(&args[1], true)?))
}

fn apply_let(args: &[MalType], env: &Env) -> MalRet {
    validate_args(args, 2, "let*");
    let mut let_env = Env::new_env(Some(env.clone()));
    match &args[0] {
        List(binds) | Vector(binds) => {
            if binds.len() % 2 != 0 {
                return Err("EOF: let bindings must be balanced!".to_string())
            }
            let mut bind_iter = binds.iter();
            while let Some(bind) = bind_iter.next() {
                if let Symbol(key) = bind {
                    let eval_val = eval(bind_iter.next().unwrap(), &mut let_env)?;
                    let_env.set(key.to_string(), eval_val);
                } else {
                    return Err("EOF: bindings must be symbols!".to_string())
                }
            }
            eval(&args[1], &mut let_env)
        }
        _ => Err("EOF: let needs bind list!".to_string())
    }

}

fn validate_args(args: &[MalType], count: usize, str: &str) -> Option<String> {
    if args.len() != count {
        Some(
         format!("EOF: invalid argument count for: {}, got: {}, need: {}",
                str, args.len(), count)
        )
    } else {
        None
    }
}
