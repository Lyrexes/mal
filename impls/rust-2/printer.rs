use fnv::FnvHashMap;
use crate::types::*;

pub fn pr_str(ast: &MalType, readably: bool) -> String{
    match ast {
        MalType::Atom(a)   => format!("(atom {})", pr_str(&(**a).borrow().clone(), readably)),
        MalType::HashMap(m,_) => print_map(m, readably),
        MalType::Vector(v,_) => {
            print_seq( &(**v), '[', ']', readably)
        }
        MalType::List(l,_) => {
            print_seq(&(**l), '(', ')', readably)
        }
        MalType::Symbol(s) => (**s).to_owned(),
        MalType::String(s) => print_string(&(**s), readably),
        MalType::Keyword(k) => print_keyword(&(**k)),
        MalType::Number(n) => n.to_string(),
        MalType::Bool(b) => print_bool(b),
        MalType::Builtin(_,_) | MalType::Lambda(_,_)
        | MalType::Macro(_,_) => "#<function>".to_owned(),
        MalType::Nil => "nil".to_owned(),
    }
}

fn print_string(str: &String, readably: bool) -> String {
    if !readably { return str.clone() }
    let mal_str = str.chars().map(|c| {
        match c {
            '\n' => "\\n".to_string(),
            '\\' => "\\\\".to_string(),
            '"'  => "\\\"".to_string(),
            _    =>  c.to_string(),
        }
    })
    .collect::<Vec<String>>()
    .join("");
    format!("\"{}\"", mal_str)
}

fn print_seq(seq: &Vec<MalType>, start_delim: char, end_delim: char, readably: bool) -> String {

    if seq.is_empty() { return format!("{}{}", start_delim, end_delim); }

    let mut seq_str = String::with_capacity(2*seq.len());

    for el in seq {
        seq_str.push_str(&format!("{} ", pr_str(el, readably)));
    }
    seq_str.pop();

    format!("{}{}{}", start_delim, seq_str, end_delim)
}

fn print_keyword(k: &String) -> String {
    let mut keyword_str = (*k).to_owned();
    keyword_str.remove(0);
    keyword_str.insert(0, ':');
    keyword_str
}

fn print_bool(b: &bool) -> String {
    if *b {
        "true".to_owned()
    } else {
        "false".to_owned()
    }
}

fn print_map(m: &FnvHashMap<String, MalType>, readably: bool) -> String {

    if m.is_empty() { return "{}".to_owned() }

    let mut map_str = String::with_capacity(4*m.len());

    for (key, value) in m.iter() {
        let new_key = {
            if key.starts_with('\u{29E}') {
                print_keyword(key)
            } else {
                print_string(key, readably)
            }
        };
        map_str.push_str(&format!("{} {} ", new_key, pr_str(value, readably)));
    }
    map_str.pop();

    format!("{}{}{}", '{', map_str, '}')
}


