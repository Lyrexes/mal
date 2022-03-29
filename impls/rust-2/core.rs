use fnv::FnvHashMap;
use std::cell::RefCell;
use std::rc::Rc;
use std::fs;
use crate::types::{MalType, MalRet, Args,MalError};
use crate::types::MalType::{Builtin, Lambda, Nil, Number, Atom, Vector, List, Bool};
use crate::{string, list, atom};
use crate::{pr_str, read_str, error_msg};
use crate::eval;


macro_rules! count {
    () => {0usize};
    ( $x:tt $($xs:tt)* ) => {1usize + count!($($xs)*)};
}

macro_rules! map {
    ($($key:expr => $val:expr),*) => {{
        let size = count!($($key)*);
        let mut map = FnvHashMap::<String, MalType>
         ::with_capacity_and_hasher(
            size,
            fnv::FnvBuildHasher::default()
        );
        $(
            map.insert($key.to_string(), $val);
        )*
        map
    }}
}

pub fn get_ns() -> FnvHashMap<String, MalType> {
    map![
        "+"           => Builtin(add),
        "-"           => Builtin(sub),
        "/"           => Builtin(div),
        "*"           => Builtin(mul),
        "prn"         => Builtin(prn),
        "pr-str"      => Builtin(prn_str),
        "str"         => Builtin(to_str),
        "println"     => Builtin(println),
        "list"        => Builtin(list),
        "list?"       => Builtin(is_list),
        "empty?"      => Builtin(is_empty),
        "count"       => Builtin(count),
        "="           => Builtin(eq),
        ">="          => Builtin(gt_eq),
        ">"           => Builtin(gt),
        "<="          => Builtin(lt_eq),
        "<"           => Builtin(lt),
        "read-string" => Builtin(read_string),
        "slurp"       => Builtin(slurp),
        "atom"        => Builtin(atom),
        "atom?"       => Builtin(is_atom),
        "deref"       => Builtin(deref),
        "reset!"      => Builtin(reset),
        "swap!"       => Builtin(swap),
        "concat"      => Builtin(concat),
        "cons"        => Builtin(cons),
        "vec"         => Builtin(vec),
        "nth"         => Builtin(nth),  
        "first"       => Builtin(first),
        "rest"        => Builtin(rest),
        "throw"       => Builtin(throw)
    ]
}

fn throw(args: Args) -> MalRet {
    validate_args(args, 1, "throw")?;
    Err(MalError::MalVal(args[0].clone()))
}

fn apply(args:Args) -> MalRet {
    validate_args_at_least(args, 2, "apply")?;
    let last = args.len()-1;
    match (args[0], args[args.len()-1]) {
        (Lambda(f), List(seq) | Vector(seq)) => {
            if seq.len() == 2 {
                Ok(f(&seq[..]))               
            } else {
                Ok(f([args[1..last], seq[..]].concat()))
            }
        }
        (Builtin(f), List(seq) | Vector(seq)) => {
            if seq.len() == 2 {
                Ok(f(&seq[..]))               
            } else {
                Ok(f([args[1..last], seq[..]].concat()))
            }
        }
        _ => error_msg!("apply expected a sequence type as last argument!")
    }
}

fn nth(args: Args) -> MalRet {
    validate_args(args, 2, "nth")?;
    match (&args[0], &args[1]) {
        (List(ref seq) | Vector(ref seq), Number(index))
            if seq.len() > *index as usize => {
            Ok(seq[*index as usize].clone())
        }
        _ => error_msg!(
            "nth failed because of type error or index out of bounds!"
            .to_owned()
        )
    }
}

fn first(args: Args) -> MalRet {
    validate_args(args, 1, "first")?;
    match args[0] {
        List(ref seq) | Vector(ref seq)
            if !seq.is_empty() => {
            Ok(seq[0].clone())
        }
        List(ref seq) | Vector(ref seq)
            if seq.is_empty() => {
            Ok(Nil)
        }
        Nil => Ok(Nil),
        _ => error_msg!(
            "first expected a sequence type!"
            .to_owned()
        )
    }
}

fn rest(args: Args) -> MalRet {
    validate_args(args, 1, "first")?;
    match args[0] {
        List(ref seq) | Vector(ref seq)
            if !seq.is_empty() => {
            Ok(list![seq[1..].to_vec()])
        }
        List(ref seq) | Vector(ref seq)
            if seq.is_empty() => {
            Ok(list![])
        }
        Nil => Ok(list![]),
        _ => error_msg!("first expected a sequence type!")
    }
}

fn vec(args:Args) -> MalRet {
    validate_args(args, 1, "vec")?;
    match args[0] {
        List(ref v) => Ok(MalType::Vector(v.clone())),
        Vector(_) => Ok(args[0].clone()),
        _ => error_msg!("vec can aonly be called with sequence tpyes!")
    }
}   

fn swap(args:Args) -> MalRet{
    validate_args_at_least(args, 2, "swap!")?;
    match (&args[0], &args[1]) {
        (Atom(a), Lambda(f)) => {
            let (ast, mut env) = f(&[
                &[(**a).borrow().clone()],
                &args[2..]
            ].concat())?;
            let new_val = eval(&ast, &mut env)?;
            a.replace(new_val.deep_copy());
            Ok(new_val)
        }
        (Atom(a), Builtin(f)) => {
            let new_val = f(&[
                &[(**a).borrow().clone()],
                &args[2..]
            ].concat())?;
            a.replace(new_val.deep_copy());
            Ok(new_val)
        }
        _ => error_msg!("swap needs a Atom and a function!"),
    }
}

fn concat(args: Args) -> MalRet {
    let mut size = 0usize;
    for arg in args.iter() {
        if let List(ref seq) | Vector(ref seq) = arg {
            size += seq.len();
        } else {
            return error_msg!("concat needs a lists or vectors!");
        }
    }
    
    let mut new_lst = Vec::with_capacity(size);

    for arg in args.iter() {
        if let List(ref seq) | Vector(ref seq)  = arg {
            new_lst.append(&mut (**seq).clone());
       }
    }
    Ok(list!(new_lst))
}

fn cons(args:Args) -> MalRet {
    if let List(ref seq) | Vector(ref seq) = args[1] {
        let mut new_seq = (**seq).clone();
        new_seq.insert(0, args[0].clone());
        Ok(list!(new_seq))
    } else {
        error_msg!("cons needs a sequence type!")
    }
}

fn reset(args:Args) ->MalRet {
    validate_args(args, 2, "reset!")?;
    if let Atom(ref a) = args[0] { 
        (**a).replace(args[1].deep_copy());
        Ok(args[1].clone())
    } else {
        error_msg!("only atoms can be reseted!")
    }
}   

fn is_atom(args: Args) ->MalRet {
    validate_args(args, 1, "atom?")?;
    Ok(Bool(if let Atom(_) = args[0] {true} else {false}))
}

fn deref(args: Args) -> MalRet {
    validate_args(args, 1, "deref")?;
    if let Atom(ref a) = args[0] {
        Ok((**a).borrow().clone())
    } else {
        error_msg!("can only deref atoms!")
    }
}

fn atom(args: Args) -> MalRet {
    validate_args(args, 1, "atom")?;
    Ok(atom!(args[0].deep_copy()))
}

fn slurp(args: Args) -> MalRet {
    validate_args(args, 1, "slurp")?;
    if let MalType::String(ref filename) = args[0] {
        let contents = match fs::read_to_string((**filename).to_owned()) {
            Ok(contents) => contents,
            Err(err) =>  return error_msg!(err)
        };
        Ok(string!(contents))
    } else {
        error_msg!("slurp expected string!")
    }
}

fn read_string(args: Args) -> MalRet {
    validate_args(args, 1, "read-string")?;
    if let MalType::String(ref str) = args[0] { 
        read_str((**str).to_owned())
    } else {
        error_msg!("read-string expected string!")
    }
}

fn lt_eq(args: Args) -> MalRet {
    num_op(args, "<=", |x, y| Ok(Bool(x <= y)))
}
fn lt(args: Args) -> MalRet {
    num_op(args, "<", |x, y| Ok(Bool(x < y)))
}
fn gt_eq(args: Args) -> MalRet {
    num_op(args, ">=", |x, y| Ok(Bool(x >= y)))
}
fn gt(args: Args) -> MalRet {
    num_op(args, ">", |x, y| Ok(Bool(x > y)))
}

fn eq(args: Args) -> MalRet {
    validate_args(args, 2, "=")?;
    Ok(Bool(args[0] == args[1]))
}

fn count(args: Args) -> MalRet {
    validate_args(args, 1, "count")?;
    match args[0] {
        List(ref seq) | Vector(ref seq) => Ok(Number(seq.len() as i64)),
        Nil => Ok(Number(0)),
        _=> error_msg!("only a seq can be counted!")
    }
}
fn is_empty(args: Args) -> MalRet {
    validate_args(args, 1, "empty?")?;
    match args[0] {
        List(ref seq) | Vector(ref seq) => Ok(Bool(seq.is_empty())),
        _=> error_msg!("only a seq can be empty!")
    }
}

fn is_list(args: Args) -> MalRet {
    validate_args(args, 1, "list?")?;
    Ok(Bool(if let List(_) = args[0] { true } else { false }))
}

fn list(args: Args) -> MalRet {
    Ok(list!(
        args.iter()
         .map(|x| x.clone())
         .collect::<Vec<_>>()
    ))
}

fn to_str(args: Args) -> MalRet { 
    Ok(string!(args.iter()
     .map(|x|pr_str(x, false))
     .collect::<Vec<_>>()
     .join("")))
}

pub fn println(args: Args) -> MalRet { 
    println!("{}",
     args.iter()
     .map(|x|pr_str(x, false))
     .collect::<Vec<_>>()
     .join(" "));
     Ok(Nil)
}

fn prn_str(args: Args) -> MalRet { 
    Ok(string!(args.iter()
     .map(|x|pr_str(x, true))
     .collect::<Vec<_>>()
     .join(" ")))
}

fn prn(args: Args) -> MalRet { 
    println!("{}",
     args.iter()
     .map(|x|pr_str(x, true))
     .collect::<Vec<_>>()
     .join(" "));
     Ok(Nil)
}

fn add(args: Args) -> MalRet { 
    num_op(args, "+", |x, y| Ok(Number(x + y)))
}

fn mul(args: Args) -> MalRet { 
    num_op(args, "*", |x, y| Ok(Number(x * y)))
}

fn sub(args: Args) -> MalRet { 
    num_op(args, "-", |x, y| Ok(Number(x - y)))
}

fn div(args: Args) -> MalRet { 
    num_op(args, "/", |x, y| {
        Ok(Number(
        x.checked_div(y)
        .ok_or(MalError::Message("invalid division!".to_owned()))?
        ))
    })
}

fn num_op(args: Args, op: &str, f: fn(i64, i64) -> MalRet) -> MalRet {
    validate_args(args, 2, op)?;
    match (&args[0],  &args[1]) {
        (Number(ref lhs), Number(ref rhs)) => f(*lhs, *rhs),
        (ref first, ref second) => error_msg!(
          format!("invalid type for operation: {}, need number got: {} and {}",
          op, pr_str(first, true), pr_str(second, true)) 
        )
    }
}

pub fn validate_args(args: &[MalType], count: usize, str: &str) -> Result<(), MalError> {
    if args.len() != count {
        error_msg!(
         format!("invalid argument count for: {}, got: {}, need: {}",
                str, args.len(), count)
        )
    } else {
        Ok(())
    }
}

pub fn validate_args_at_least(args: &[MalType], count: usize, str: &str) -> Result<(), MalError> {
    if args.len() < count {
        error_msg!(
         format!("invalid argument count for: {}, got: {}, need: {} ore more!",
                str, args.len(), count)
        )
    } else {
        Ok(())
    }
}

pub fn validate_arg_bounds(args: &[MalType], min: usize, max: usize, str: &str) -> Result<(), MalError> {
    if args.len() < min || args.len() > max {
        error_msg!(
         format!("invalid argument count for: {}, got: {}, need: {} and atleast: {}",
                str, args.len(), min, max)
        )
    } else {
        Ok(())
    }
}