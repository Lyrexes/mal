use env::{Env, EnvInterface};
use std::{rc::Rc};
use printer::pr_str;
use reader::read_str;
use rustyline::error::ReadlineError;
use types::{MalType::{Number,Symbol, Vector,List, Lambda, Builtin, HashMap, Nil, Bool},
             MalRet, MalType, MalError};
use crate::core::*;
mod reader;
mod types;
mod printer;
mod env;
mod core;



fn main() {
    let mut repl_env = Env::with_map(None, get_ns());
    inject_code(&mut repl_env, "(def! not (fn* (a) (if a false true)))");
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

fn inject_code(env: &mut Env, code: &str) {
    match read_eval(code.to_string(), env) {
        Ok(_) => (),
        Err(e) => panic!("Error injecting code: {}",e)
    }
}

fn read_eval(input: String, env: &mut Env) -> MalRet {
    eval(&read(input)?, env)
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
                Symbol(s) if **s == "def!" => apply_def(&list[1..], env),
                Symbol(s) if **s == "let*" => apply_let(&list[1..], env),
                Symbol(s) if **s == "do"   => apply_do(&list[1..], env),
                Symbol(s) if **s == "if"   => apply_if(&list[1..], env),
                Symbol(s) if **s == "fn*"  => create_lambda(&list[1..], env),
                _ => if let List(eval_list) = eval_ast(ast, env)? {
                        if let Lambda(ref l) = eval_list[0] {
                            let (l_ast, mut l_env) = l(&eval_list[1..])?;
                            return eval(&l_ast, &mut l_env)
                        }
                    Ok(eval_list[0].apply(&eval_list[1..])?)
                } else {
                    unreachable!("EOF: expected list!");
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
    validate_args(args, 2, "def!")?;
    if let Symbol(key) = &args[0] {
        let eval_val = eval(&args[1], env)?;
        env.set((**key).clone(), eval_val.clone());
        return Ok(eval_val);
    }
    Err(format!("EOF: binding var must be a symbol! got: {}", pr_str(&args[1], true)?))
}

fn apply_let(args: &[MalType], env: &Env) -> MalRet {
    validate_args(args, 2, "let*")?;
    let mut let_env = Env::new_env(Some(env.clone()));
    match args[0] {
        List(ref binds) | Vector(ref binds) => {
            if binds.len() % 2 != 0 {
                return Err("EOF: let bindings must be balanced!".to_string())
            }
            let_env.reserve(binds.len() / 2 as usize);
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

fn apply_do(args: &[MalType], env: &mut Env) -> MalRet {
    validate_args_at_least(args, 1, "do")?;
    if args.len() == 1 { return eval(&args[0], env) }
    for arg in args[..args.len()-1].iter() {
        eval(arg, env)?;
    }
    eval(&args[args.len()-1], env)
}

fn apply_if(args: &[MalType], env: &mut Env) -> MalRet {
    validate_arg_bounds(args, 2, 3,"if")?;
    let has_else = args.len() == 3;
    match eval(&args[0], env)? {
        Bool(cond) => {
            if cond {
                eval(&args[1], env)
            } else if has_else {
                eval(&args[2], env)
            } else {
                Ok(MalType::Nil)
            }
        }
        Nil => {
            if has_else {
                eval(&args[2], env) 
            } else {
                Ok(MalType::Nil)
            }
        }
        _ => eval(&args[1], env)
    }
}
/** 
 * Lambda:  (fn* (params..) body..)
 * Example: (fn* (a b) (+ b a))
 * Example: (fn* (x) x)
 * Example: ((fn* (a & variadic) (list? variadic)) 1 2 3) => true
**/

fn create_lambda(args: &[MalType], env: &Env) -> MalRet {
    match args[0] {
        List(ref params) | Vector(ref params) => {

            let all_sym = params.iter()
             .all(|x| if let Symbol(_) = x {true} else {false});

            if !all_sym{
                return Err(
                    "EOF: invalid fucntion paramter must be a symbol!"
                    .to_string()
                )
            }
            let old_env = env.clone();
            let body = args[1].clone();
            let mut lambda_params = params.clone()
                .iter().map(|x| if let Symbol(sym) = x
                     {Some(sym.clone())} else{None})
                .flatten()
                .collect::<Vec<_>>();

            if let Some(vari_i) = lambda_params.iter().position(|x| **x == "&") {
                lambda_params.remove(vari_i);
                if *lambda_params.last().unwrap() != lambda_params[vari_i] {
                    return Err(
                    "EOF: after a variadic parameter cant be another parameter!"
                    .to_string()
                    )
                }
                Ok(Lambda(Rc::new(move |arguments: &[MalType]| -> Result<(MalType, Env), MalError> {
                    let mut lambda_env = Env::new_env(Some(old_env.clone()));
                    lambda_env.reserve(lambda_params.len());
                    lambda_env.insert(&lambda_params[..vari_i], &arguments[..vari_i]);
                    lambda_env.set((*lambda_params[vari_i]).clone(), list![arguments[vari_i..].to_vec()]);
                    Ok((body.clone(), lambda_env))
                })))
            }
            else {
                Ok(Lambda(Rc::new(move |arguments: &[MalType]| -> Result<(MalType, Env), MalError> {
                    if lambda_params.len() != arguments.len() {
                        return Err(
                            "EOF: invalid lambda call not enough arguments!".to_string()
                        )
                    }
                    Ok((body.clone(), Env::with_binds(
                        Some(old_env.clone()),
                        &*lambda_params,
                        arguments
                    )))
                })))
            }
        }
        _ => Err(
                "EOF: invalid lambda call, parameters must be a list or vector!"
                .to_string()
            )
    }
}

