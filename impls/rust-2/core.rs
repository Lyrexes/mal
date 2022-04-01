use fnv::FnvHashMap;
use std::cell::RefCell;
use std::rc::Rc;
use std::fs;
use crate::types::{MalType, MalRet, Args,MalError};
use crate::types::MalType::{Builtin, HashMap, Macro, Symbol, Keyword, Lambda, Nil, Number, Atom, Vector, List, Bool};
use crate::types::MalType::String as MalString;
use crate::{string, list, builtin, map, atom, symbol, vector, keyword};
use crate::{pr_str, read_str, error_msg, input};
use std::time::SystemTime;
use crate::eval;


macro_rules! count {
    () => {0usize};
    ( $x:tt $($xs:tt)* ) => {1usize + count!($($xs)*)};
}

macro_rules! ns_map {
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
    ns_map![
        "+"           => builtin!(add),
        "-"           => builtin!(sub),
        "/"           => builtin!(div),
        "*"           => builtin!(mul),
        "prn"         => builtin!(prn),
        "pr-str"      => builtin!(prn_str),
        "str"         => builtin!(to_str),
        "println"     => builtin!(println),
        "list"        => builtin!(list),
        "list?"       => builtin!(is_list),
        "empty?"      => builtin!(is_empty),
        "count"       => builtin!(count),
        "="           => builtin!(eq),
        ">="          => builtin!(gt_eq),
        ">"           => builtin!(gt),
        "<="          => builtin!(lt_eq),
        "<"           => builtin!(lt),
        "read-string" => builtin!(read_string),
        "slurp"       => builtin!(slurp),
        "atom"        => builtin!(atom),
        "atom?"       => builtin!(is_atom),
        "deref"       => builtin!(deref),
        "reset!"      => builtin!(reset),
        "swap!"       => builtin!(swap),
        "concat"      => builtin!(concat),
        "cons"        => builtin!(cons),
        "vec"         => builtin!(vec),
        "nth"         => builtin!(nth),  
        "first"       => builtin!(first),
        "rest"        => builtin!(rest),
        "throw"       => builtin!(throw),
        "apply"       => builtin!(apply),
        "map"         => builtin!(map),
        "nil?"        => builtin!(is_nil),
        "true?"       => builtin!(is_true),
        "false?"      => builtin!(is_false),
        "symbol?"     => builtin!(is_symbol),
        "vals"        => builtin!(vals),
        "keys"        => builtin!(keys),
        "symbol"      => builtin!(symbol),
        "keyword"     => builtin!(keyword),
        "keyword?"    => builtin!(is_keyword),
        "vector"      => builtin!(vector),
        "vector?"     => builtin!(is_vector),
        "sequential?" => builtin!(is_sequential),
        "hash-map"    => builtin!(hash_map),
        "map?"        => builtin!(is_map),
        "assoc"       => builtin!(assoc),
        "dissoc"      => builtin!(dissoc),
        "get"         => builtin!(get),
        "contains?"   => builtin!(contains),
        "readline"    => builtin!(read_line),
        "time-ms"     => builtin!(time_ms),
        "meta"        => builtin!(meta),
        "with-meta"   => builtin!(with_meta),
        "fn?"         => builtin!(is_fn),
        "string?"     => builtin!(is_string),
        "number?"     => builtin!(is_number),
        "seq"         => builtin!(seq),
        "conj"        => builtin!(conj),
        "macro?"       => builtin!(is_macro)
    ]
}

fn conj(args: Args) -> MalRet {
    validate_args_at_least(args, 2, "conj")?;
    match args[0] {
        List(ref seq,_) => {
            let mut new_list = vec![];
            new_list.extend((args[1..]).iter().map(|x| x.clone()).rev());
            new_list.extend_from_slice(&seq[..]);
            Ok(list!(new_list))
        },
        Vector(ref seq,_) => {
            Ok(vector!([&seq[..],&args[1..]].concat()))
        },
        _ => error_msg!("conj expects a list or vector!")
    }
}

fn is_number(args: Args) -> MalRet {
    validate_args(args, 1, "number?")?;
    if let Number(_) = args[0] {
        Ok(Bool(true))
    } else {
        Ok(Bool(false))
    }
}

fn is_string(args: Args) -> MalRet {
    validate_args(args, 1, "string?")?;
    if let MalString(_) = args[0] {
        Ok(Bool(true))
    } else {
        Ok(Bool(false))
    }
}

fn is_fn(args: Args) -> MalRet {
    validate_args(args, 1, "fn?")?;
    if let Lambda(_,_) | Builtin(_,_) = args[0] {
        Ok(Bool(true))
    } else {
        Ok(Bool(false))
    }
}

fn is_macro(args: Args) -> MalRet {
    validate_args(args, 1, "macro?")?;
    if let Macro(_,_) = args[0] {
        Ok(Bool(true))
    } else {
        Ok(Bool(false))
    }
}

fn with_meta(args: Args) -> MalRet {
    validate_args(args, 2, "with_meta?")?;
    match &args[0] {
        HashMap(m,_) => Ok(HashMap(m.clone(), Rc::new(args[1].clone()))),
        Vector(v,_) =>  Ok(Vector(v.clone(), Rc::new(args[1].clone()))),
        List(l,_) => Ok(List(l.clone(), Rc::new(args[1].clone()))),
        Builtin(b,_) => Ok(Builtin(b.clone(), Rc::new(args[1].clone()))),
        Lambda(l,_) => Ok(Lambda(l.clone(), Rc::new(args[1].clone()))),
        Macro(m,_) => Ok(Macro(m.clone(), Rc::new(args[1].clone()))),
        _ => error_msg!("with-meta expects a meta-type!")
    }
}
fn meta(args: Args) -> MalRet {
    validate_args(args, 1, "meta")?;
    match &args[0] {
        HashMap(_,meta) | Vector(_,meta) |
        List(_,meta)    | Builtin(_,meta) |
        Lambda(_,meta)  | Macro(_,meta) => Ok((**meta).clone()),
        _ => error_msg!("meta expects a meta type!")
    }
}

fn time_ms(args: Args) -> MalRet {
    validate_args(args, 0, "time-ms")?;
    match SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH) {
            Ok(n) => Ok(Number(n.as_millis() as i64)),
            Err(s) => error_msg!(s.to_string()),
        }
}

fn seq(args: Args) -> MalRet {
    validate_args(args, 1, "seq")?;
    match args[0] {
        List(ref seq,_) | Vector(ref seq,_) => {
            if seq.is_empty() { 
                Ok(Nil)
            } else {
                Ok(List(seq.clone(), Rc::new(Nil)))
            }
        },
        MalString(ref str) => {
            if str.is_empty() { 
                Ok(Nil)
            } else {
                Ok(list!((**str).chars()
                 .map(|c| string!(c))
                 .collect::<Vec<_>>()))
            }
        },
        Nil => Ok(Nil),
        _ => error_msg!("seq expects a string list vec and nil!")
    }
}

fn read_line(args:Args) -> MalRet {
    validate_args(args, 1, "readline")?;
    if let MalString(ref str) = args[0] {
        match input(str.as_str()) {
            Ok(str) => Ok(string!(str)),
            _ => Ok(Nil)
        }
    } else {
        error_msg!("readline expects a string as prompt!")
    }
}

fn vals(args: Args) -> MalRet {
    validate_args(args, 1, "vals")?;
    if let HashMap(ref map,_) = args[0] {
        Ok(list!(map.values().map(|x| x.clone()).collect::<Vec<_>>()))
    } else {
        error_msg!("keys expects a map!")
    }
}

fn keys(args: Args) -> MalRet {
    validate_args(args, 1, "keys")?;
    if let HashMap(ref map,_) = args[0] {
        Ok(list!(map.keys().map(|x| {
            if x.starts_with('\u{29E}') {
                keyword!(x)
            } else {
                string!(x)
            }
        }).collect::<Vec<_>>()))
    } else {
        error_msg!("keys expects a map!")
    }
}

fn contains(args:Args) -> MalRet {
    validate_args(args, 2, "contains?")?;
    match (&args[0], &args[1]) {
        (HashMap(map,_), MalString(key) | Keyword(key)) => {
            Ok(Bool(map.contains_key(&(**key))))
        }
        _ => error_msg!("get expects a map and a valid key type!")
    }
}

fn symbol(args:Args) -> MalRet {
    validate_args(args, 1, "symbol")?;
    if let MalString(ref str) = args[0] {
        Ok(symbol!((**str).clone()))
    } else {
        error_msg!("symbol expects a string!")
    }
}

fn keyword(args:Args) -> MalRet {
    validate_args(args, 1, "keyword")?;
    match args[0] {
        MalString(ref str) => {
            Ok(keyword!(format!("{}{}", '\u{29E}', (**str).clone())))
        }
        Keyword(ref k) => Ok(keyword!((**k).clone())),
        _ => error_msg!("keyword expects a string!")
    }
}

fn is_keyword(args:Args) -> MalRet {
    validate_args(args, 1, "keyword?")?;
    if let Keyword(_) = args[0] {
        Ok(Bool(true))
    } else {
        Ok(Bool(false))
    }

}

fn is_sequential(args: Args) -> MalRet {
    validate_args(args, 1, "sequential?")?;
    if let Vector(_,_) | List(_,_) = args[0] {
        Ok(Bool(true))
    } else {
        Ok(Bool(false))
    }
}

fn vector(args: Args) -> MalRet {
    let mut v = Vec::with_capacity(args.len());
    for arg in args.iter() {
        v.push(arg.deep_copy());
    }
    Ok(vector!(v))
}

fn is_vector(args: Args) -> MalRet {
    validate_args(args, 1, "vector?")?;
    if let Vector(_,_) = args[0] {
        Ok(Bool(true))
    } else {
        Ok(Bool(false))
    }
}

fn hash_map(args: Args) -> MalRet {
    if args.len() % 2 != 0 {
        return error_msg!("hash map requires an even number of arguments")
    } 
    let mut map = fnv::FnvHashMap
        ::with_capacity_and_hasher(
            args.len()/2,
            fnv::FnvBuildHasher::default()
    );
    let mut map_iter = args.iter();
    while let Some(val) = map_iter.next() {
        if let MalString(key) | Keyword(key) = val {
            map.insert((**key).clone(), map_iter.next().unwrap().deep_copy());
        } else {
            return error_msg!("hashmap expects key to be string or keyword!");
        }
    }
    Ok(map!(map))
}

fn is_map(args:Args) -> MalRet {
    validate_args(args, 1, "map?")?;
    if let HashMap(_,_) = args[0] {
        Ok(Bool(true))
    } else {
        Ok(Bool(false))
    }
}

fn get(args: Args) -> MalRet {
    validate_args(args, 2, "get")?;
    match (&args[0], &args[1]) {
        (HashMap(map,_), MalString(key) | Keyword(key)) => {
            if let Some(val) = map.get(&(**key)) {
                Ok(val.clone())
            } else {
                Ok(Nil)
            }
        }
        (Nil, _) => Ok(Nil),
        _ => error_msg!("get expects a map and a valid key type!")
    }
}

fn dissoc(args:Args) -> MalRet{
    validate_args_at_least(args, 1, "dissoc")?;
    if let HashMap(ref map,_) = args[0] {
        if args.len() == 1 { return Ok(args[0].deep_copy()) }
        let mut new_map = (**map).clone();
        for arg in args[1..].iter() {
            if let Keyword(key) | MalString(key) = arg {
                new_map.remove(&(**key));
            } else {
                return error_msg!("dissoc expects a string or keyword as key!")
            }
        }
        Ok(map!(new_map))
    } else {
        error_msg!("dissoc expects a hashmap!")
    }
}

fn assoc(args: Args) -> MalRet {
    validate_args_at_least(args, 1, "assoc")?;
    if let HashMap(ref map,_) = args[0] {
        if args.len() == 1 { return Ok(args[0].deep_copy()) }
        if (args.len()-1) % 2 == 0 {
            let mut new_map = (**map).clone();
            new_map.reserve((args.len() - 1) / 2);
            let mut map_iter = args.iter();
            map_iter.next();
            while let Some(val) = map_iter.next() {
                if let MalString(key) | Keyword(key) = val {
                    new_map.insert((**key).clone(), map_iter.next().unwrap().deep_copy());
                } else {
                    return error_msg!("hashmap expects key to be string or keyword!");
                }
            }
            Ok(map!(new_map))
        } else {
            error_msg!("assoc expected an even amoun of keys and valuse!")
        }
    } else {
        error_msg!("assoc expected a HashMap as first argument!")
    }
}

fn is_nil(args:Args) -> MalRet {
    validate_args(args, 1, "nil?")?;
    if let Nil = args[0] {
        Ok(Bool(true))
    } else {
        Ok(Bool(false))
    }
}


fn is_true(args:Args) -> MalRet {
    validate_args(args, 1, "true?")?;
    if let Bool(b) = args[0] {
        Ok(Bool(b))
    } else {
        Ok(Bool(false))
    }
}

fn is_false(args:Args) -> MalRet {
    validate_args(args, 1, "false?")?;
    if let Bool(b) = args[0] {
        Ok(Bool(!b))
    } else {
        Ok(Bool(false))
    }
}

fn is_symbol(args:Args) -> MalRet {
    validate_args(args, 1, "symbol?")?;
    if let Symbol(_) = args[0] {
        Ok(Bool(true))
    } else {
        Ok(Bool(false))
    }
}

fn map(args: Args) -> MalRet {
    validate_args(args, 2, "map")?;
    match (&args[0], &args[1]) {
        (Lambda(f,_), List(seq,_) | Vector(seq,_)) => {
            let mut lst = Vec::with_capacity(seq.len());
            let (mut ast, mut env);
            for el in seq.iter() {
                (ast, env) = f(&[el.clone()])?;
                lst.push(eval(&ast, &mut env)?);
            }
            Ok(list!(lst))
        }
        (Builtin(f,_), List(seq,_) | Vector(seq,_)) => {
            let mut lst = Vec::with_capacity(seq.len());
            for el in seq.iter() {
                lst.push(f(&[el.clone()])?);
            }
            Ok(list!(lst))
        }
        _ => error_msg!("map expected a sequence and a function!")
    }
    
}

fn throw(args: Args) -> MalRet {
    validate_args(args, 1, "throw")?;
    Err(MalError::MalVal(args[0].clone()))
}

fn apply(args:Args) -> MalRet {
    validate_args_at_least(args, 2, "apply")?;
    let last = args.len()-1;
    match (&args[0], &args[args.len()-1]) {
        (Lambda(f,_), List(seq,_) | Vector(seq,_)) => {
            let (ast, mut env);
            if args.len() == 2 {
                (ast, env) = f(&seq[..])?;
            } else {
                (ast, env) = f(&[&args[1..last], &seq[..]].concat())?;
            }
            eval(&ast, &mut env)
        }
        (Builtin(f,_), List(seq,_) | Vector(seq,_)) => {
            if args.len() == 2 {
                f(&seq[..])
            } else {
                f(&[&args[1..last], &seq[..]].concat())
            }
        }
        _ => error_msg!("apply expected a sequence type as last argument!")
    }
}

fn nth(args: Args) -> MalRet {
    validate_args(args, 2, "nth")?;
    match (&args[0], &args[1]) {
        (List(ref seq,_) | Vector(ref seq, _), Number(index))
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
        List(ref seq,_) | Vector(ref seq, _)
            if !seq.is_empty() => {
            Ok(seq[0].clone())
        }
        List(ref seq,_) | Vector(ref seq, _)
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
        List(ref seq,_) | Vector(ref seq, _)
            if !seq.is_empty() => {
            Ok(list![seq[1..].to_vec()])
        }
        List(ref seq,_) | Vector(ref seq, _)
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
        List(ref v,_) => Ok(MalType::Vector(v.clone(), Rc::new(Nil))),
        Vector(_,_) => Ok(args[0].clone()),
        _ => error_msg!("vec can aonly be called with sequence tpyes!")
    }
}   

fn swap(args:Args) -> MalRet{
    validate_args_at_least(args, 2, "swap!")?;
    match (&args[0], &args[1]) {
        (Atom(a), Lambda(f,_)) => {
            let (ast, mut env) = f(&[
                &[(**a).borrow().clone()],
                &args[2..]
            ].concat())?;
            let new_val = eval(&ast, &mut env)?;
            a.replace(new_val.deep_copy());
            Ok(new_val)
        }
        (Atom(a), Builtin(f,_)) => {
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
        if let List(ref seq,_) | Vector(ref seq, _) = arg {
            size += seq.len();
        } else {
            return error_msg!("concat needs a lists or vectors!");
        }
    }
    
    let mut new_lst = Vec::with_capacity(size);

    for arg in args.iter() {
        if let List(ref seq,_) | Vector(ref seq, _)  = arg {
            new_lst.append(&mut (**seq).clone());
       }
    }
    Ok(list!(new_lst))
}

fn cons(args:Args) -> MalRet {
    if let List(ref seq,_) | Vector(ref seq, _) = args[1] {
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
    if let MalString(ref filename) = args[0] {
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
    if let MalString(ref str) = args[0] { 
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
        List(ref seq,_) | Vector(ref seq, _) => Ok(Number(seq.len() as i64)),
        Nil => Ok(Number(0)),
        _=> error_msg!("only a seq can be counted!")
    }
}
fn is_empty(args: Args) -> MalRet {
    validate_args(args, 1, "empty?")?;
    match args[0] {
        List(ref seq,_) | Vector(ref seq, _) => Ok(Bool(seq.is_empty())),
        _=> error_msg!("only a seq can be empty!")
    }
}

fn is_list(args: Args) -> MalRet {
    validate_args(args, 1, "list?")?;
    Ok(Bool(if let List(_,_) = args[0] { true } else { false }))
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