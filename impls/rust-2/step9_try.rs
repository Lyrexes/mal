use env::{Env, EnvInterface};
use std::{rc::Rc};
use std::collections::HashMap as stdMap;
use printer::pr_str;
use reader::read_str;
use rustyline::error::ReadlineError;
use types::{MalType::{HashMap, Symbol, Vector,List, Lambda, Nil, Bool, Macro},
             MalRet, MalType, MalError};
use crate::core::{get_ns, validate_args, validate_args_at_least, validate_arg_bounds};
mod reader;
mod types;
mod printer;
mod env;
mod core;

fn main() {

    let mut repl_env = init_repl_env();
    
    if std::env::args().count() > 1usize { 
        return run_file(
         std::env::args()
          .collect::<Vec<_>>(),
         &mut repl_env
        );
    }
    loop {
        match input("user> ") {
            Ok(line) => 
            println!("{}", match rep(line, &mut repl_env) {
                Ok(str) => str,
                Err(err) => match err {
                    MalError::MalVal(v) => format!("uncaught exception: {}",pr_str(&v, true)),
                    MalError::Message(msg) => format!("EOF: {}", msg),
                }
            }),
            _ => return,
        }
    }
}

fn init_repl_env() -> Env {
    let mut repl_env = Env::with_map(None, get_ns());
    let env_ = repl_env.clone();
    repl_env.set("*ARGV*".to_string(), list![]);
    repl_env.set("eval".to_string(), Lambda(Rc::new(move |args| {
        validate_args(args, 1, "eval")?;
        Ok((args[0].clone(), env_.clone()))
    })));
    inject_code_str(&mut repl_env, "(def! not (fn* (a) (if a false true)))");
    inject_code_str(&mut repl_env, r#"(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))"#);
    inject_code_str(&mut repl_env,
        "(defmacro! cond 
            (fn* (& xs) 
                (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1)
                    (nth xs 1) (throw \"odd number of forms to cond\"))
                (cons 'cond (rest (rest xs)))))
            )
        )"
    );
    repl_env
}

fn run_file(args: Vec<String>, env: &mut Env) {
    if args.len() >= 3 {
        let argv = args[2..].iter()
         .map(|s| string!(s))
         .collect::<Vec<_>>();
        env.set("*ARGV*".to_string(), list![argv]);
    }
    inject_code(env, format!("(load-file \"{}\")", args[1]));
}

fn inject_code_str(env: &mut Env, code: &str) {
    read_eval(code.to_string(), env)
     .expect("Error injecting code!");
}

fn inject_code(env: &mut Env, code: String) {
    read_eval(code.to_string(), env)
     .expect("Error injecting code!");
}
fn read_eval(input: String, env: &mut Env) -> MalRet {
    eval(&read(input)?, env)
}

fn input(prompt: &str) -> Result<String, ReadlineError> {
    let mut rl = rustyline::Editor::<()>::new();
    let input = rl.readline(prompt);
    input
}

fn rep(string: String, env: &mut Env) -> Result<String, MalError> {
    Ok(print(eval(&read(string)?,env)?))
}

fn read(string: String) -> MalRet  {
    read_str(string)
}

fn eval(ast: &MalType, env: &mut Env) -> MalRet {
    let mut curr_ast = ast.clone();
    let mut curr_env = env.clone();
    loop {
        if let List(mut list) = curr_ast.clone() {

            curr_ast = macroexpand(&[curr_ast], &mut curr_env)?;

            match curr_ast.clone() {
                List(new_list) => list = new_list,
                _=> return eval_ast(&curr_ast, &mut curr_env)
            }

            if list.is_empty() { return Ok(curr_ast.clone()) }
            let first = &list[0];
            match first {
                Symbol(s) if **s == "def!" => {
                    return apply_def(&list[1..], &mut curr_env);
                },
                Symbol(s) if **s == "defmacro!" => {
                    return apply_defmacro(&list[1..], &mut curr_env);
                },
                Symbol(s) if **s == "let*" => {
                    (curr_ast, curr_env) = apply_let(&list[1..], &mut curr_env)?;
                    continue;
                },
                Symbol(s) if **s == "do" => {
                    curr_ast = apply_do(&list[1..], &mut curr_env)?;
                    continue;
                },
                Symbol(s) if **s == "if" => {
                    curr_ast = apply_if(&list[1..], &mut curr_env)?;
                    continue;
                },
                Symbol(s) if **s == "quasiquote" => {
                    curr_ast = quasiquote(&list[1..])?;
                    continue;
                }
                Symbol(s) if **s == "quote" =>{
                    validate_args(&list, 2, "quote")?;
                    return Ok(list[1].clone())
                }
                Symbol(s) if **s == "try*" => return apply_try_catch(&list[1..], &mut curr_env),
                Symbol(s) if **s == "macroexpand" => return macroexpand(&list[1..], &curr_env),
                Symbol(s) if **s == "quasiquoteexpand" => return Ok(quasiquote(&list[1..])?),
                Symbol(s) if **s == "fn*"  => return create_lambda(&list[1..], &curr_env),
                _ => {
                    if let List(eval_list) = eval_ast(&curr_ast, &mut curr_env)? {
                        if let Lambda(ref l) = eval_list[0] {
                            (curr_ast, curr_env) = l(&eval_list[1..])?;
                            continue
                        }
                        return Ok(eval_list[0].apply(&eval_list[1..])?)
                    } else {
                        unreachable!("expected list!");
                    }
                }
            }
        }
        return eval_ast(&curr_ast, &mut curr_env)
    }
}

fn eval_ast(ast: &MalType, env: &mut Env) -> MalRet {
    match ast {
        Symbol(sym) => {
            Ok(env.get(&(**sym))
             .ok_or(MalError::Message(format!("'{}' not found", sym.to_string())))?
             .clone()
            )
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
            let mut new_map = stdMap::with_capacity_and_hasher(
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

/*
* try/catch: (try* A (catch* B C))
* Example:   (try* (/ 1 0) (catch* B (println B)))
*/
fn apply_try_catch(args: &[MalType], env: &mut Env) -> MalRet {
    validate_arg_bounds(args, 1, 2, "try*/catch*")?;
    match eval(&args[0], env) {
        Ok(val_a) => Ok(val_a),
        Err(error) => {
            let has_catch = {
                if args.len() < 2 {false}
                else {
                    args[1].is_form_call("catch*", 2)
                }
            };
            if !has_catch { return Err(error) }
            let mal_err = match error {
                MalError::MalVal(v) => v,
                MalError::Message(msg) => string!(msg),
            };
            let sym = if let Symbol(s) = args[1].nth(1) { s } else {
                return error_msg!("catch binding must be a symbol!")
            };
            let mut catch_env = Env::with_binds(Some(env.clone()), &[sym], &[mal_err]);
            eval(&args[1].nth(2), &mut catch_env)
        },
    }
}

fn quasiquote(args: &[MalType]) -> MalRet {
    validate_args(args, 1, "quasiquote")?;
    let ast  = &args[0];
    if ast.is_form_call("unquote", 1) { 
        return Ok(ast.nth(1))
    }
    if let List(lst) | Vector(lst)  = ast {
        let mut result = Vec::new();
        for elt in lst.iter().rev() {
            if elt.is_form_call("splice-unquote", 1) {
                result = vec![
                    symbol!("concat"),
                    elt.nth(1),
                    list!(result)
                ];
            
            } else {
                result = vec![
                    symbol!("cons"),
                    quasiquote(&[elt.clone()])?,
                    list!(result)
                ];
            }
        }
        if let Vector(_) = ast {
            Ok(list!(vec![
                symbol!("vec"),
                list!(result)
            ]))
        } else {
            Ok(list!(result))
        }
    } else {
        match ast {
            HashMap(_) | Symbol(_) => {
                return Ok(list!(vec![
                    symbol!("quote".to_string()),
                    ast.clone()
                ]))
            },
            _ => Ok(ast.clone())
        }
    }
}

fn apply_def(args: &[MalType], env: &mut Env) -> MalRet {
    validate_args(args, 2, "def!")?;
    if let Symbol(ref key) = args[0] {
        let eval_val = eval(&args[1], env)?;
        env.set((**key).clone(), eval_val.clone());
        return Ok(eval_val);
    }
    error_msg!(format!("binding var must be a symbol! got: {}", pr_str(&args[1], true)))
}

fn apply_defmacro(args: &[MalType], env: &mut Env) -> MalRet {
    validate_args(args, 2, "def!")?;
    if let Symbol(ref key) = args[0] {
        let eval_val = eval(&args[1], env)?;
        if let Lambda(l) = eval_val {
            env.set((**key).clone(), Macro(l.clone()));
            Ok(Macro(l.clone()))
        } else {
            error_msg!("Macros must be functions!")
        }
    } else {
        error_msg!(format!("binding var must be a symbol! got: {}", pr_str(&args[1], true)))
    }
}

fn is_macro_call(ast: &MalType, env: &Env) -> bool {
    match ast {
        List(lst) if !lst.is_empty() => {
            if let Symbol(ref key) = lst[0] {
                if let Some(Macro(_))= env.get(key) {
                    return true
                }
            }
        }
        _ => ()
    }
    false
}

fn macroexpand(args: &[MalType], env:&Env) -> MalRet {
    validate_args(args, 1, "macroexpand")?;
    let mut curr_ast = args[0].clone();
    while is_macro_call(&curr_ast, env) {
        if let List(ref lst) = curr_ast {
            if let Symbol(ref key) = lst[0] {
                if let Some(Macro(m)) = env.get(key) {
                    let (m_ast, mut m_env) =  m(&lst[1..])?;
                    curr_ast = eval(&m_ast, &mut m_env)?;
                }
            }
        }
    }
    return Ok(curr_ast)
}

fn apply_let(args: &[MalType], env: &Env) -> Result<(MalType, Env), MalError> {
    validate_args(args, 2, "let*")?;
    let mut let_env = Env::new_env(Some(env.clone()));
    match args[0] {
        List(ref binds) | Vector(ref binds) => {
            if binds.len() % 2 != 0 {
                return error_msg!("let bindings must be balanced!")
            }
            let_env.reserve(binds.len() / 2 as usize);
            let mut bind_iter = binds.iter();
            while let Some(bind) = bind_iter.next() {
                if let Symbol(key) = bind {
                    let eval_val = eval(bind_iter.next().unwrap(), &mut let_env)?;
                    let_env.set(key.to_string(), eval_val);
                } else {
                    return error_msg!("bindings must be symbols!")
                }
            }
            Ok((args[1].clone(), let_env))
        }
        _ => error_msg!("let needs bind list!")
    }

}

fn apply_do(args: &[MalType], env: &mut Env) -> Result<MalType, MalError> {
    validate_args_at_least(args, 1, "do")?;
    for arg in args[..args.len()-1].iter() {
        eval(arg, env)?;
    }
    Ok(args[args.len()-1].clone())
}


fn apply_if(args: &[MalType], env: &mut Env) -> MalRet {
    validate_arg_bounds(args, 2, 3,"if")?;
    let has_else = args.len() == 3;
    match eval(&args[0], env)? {
        Bool(cond) => {
            if cond {
                Ok(args[1].clone())
            } else if has_else {
                Ok(args[2].clone())
            } else {
                Ok(MalType::Nil)
            }
        }
        Nil => {
            if has_else {
                Ok(args[2].clone())
            } else {
                Ok(MalType::Nil)
            }
        }
        _ => Ok(args[1].clone())
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
                return error_msg!("invalid fucntion paramter must be a symbol!")
            }
            let old_env = env.clone();
            let body = args[1].clone();
            let mut lambda_params = params.clone()
                .iter().map(|x| {
                    if let Symbol(sym) = x {
                        return Some(sym.clone())
                    } 
                    None
                })
                .flatten()
                .collect::<Vec<_>>();

            if let Some(vari_i) = lambda_params.iter().position(|x| **x == "&") {
                lambda_params.remove(vari_i);
                if *lambda_params.last().unwrap() != lambda_params[vari_i] {
                    return error_msg!("after a variadic parameter cant be another parameter!")
                }
                Ok(Lambda(Rc::new(move |arguments: &[MalType]| -> Result<(MalType,Env), MalError> {
                    let mut lambda_env = Env::new_env(Some(old_env.clone()));
                    lambda_env.reserve(lambda_params.len());
                    lambda_env.insert(&lambda_params[..vari_i], &arguments[..vari_i]);
                    lambda_env.set((*lambda_params[vari_i]).clone(), list![arguments[vari_i..].to_vec()]);
                    Ok((body.clone(),  lambda_env.clone()))
                })))
            } else {
                Ok(Lambda(Rc::new(move |arguments: &[MalType]| -> Result<(MalType,Env), MalError> {
                    if lambda_params.len() != arguments.len() {
                        return error_msg!("invalid lambda call not enough arguments!")
                    }
                    Ok((body.clone(), Env::with_binds(
                        Some(old_env.clone()),
                        &*lambda_params,
                        arguments 
                    )))
                })))
            }
        }
        _ => error_msg!("invalid lambda call, parameters must be a list or vector!")
    }
}

