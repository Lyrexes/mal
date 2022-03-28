use std::{collections::VecDeque, rc::Rc};
use fnv::FnvHashMap;
use lazy_static::lazy_static;
use regex::Regex;
use crate::{types::*, symbol, list, error_msg};

pub struct TokenStream {
    tokens: VecDeque<String>, 
}

impl TokenStream {
    fn new(input: String) -> TokenStream {
        TokenStream { tokens: tokenize(input) }
    }

    fn next(&mut self) -> Option<String> {
        self.tokens.pop_front()
    }

    fn peek(&self) -> Option<&String> {
        self.tokens.front()
    }

    fn pop(&mut self) {
        self.tokens.pop_front();
    }
    
}

pub fn read_str(input: String) -> MalRet {
    read_form(&mut TokenStream::new(input))
}

fn read_form(tokens: &mut TokenStream) -> MalRet {
    let first_char = tokens.peek()
        .ok_or(MalError::Message("EOF empty tokenstream".to_string()))?
        .chars().next().
         ok_or(MalError::Message("EOF empty token".to_string()))?;
    match  first_char {
        '(' => read_sequence(tokens, ")"),
        '[' => read_sequence(tokens, "]"),
        '{' => read_map(tokens),
        '"' => read_string(tokens),
        ';' => ignore_comment(tokens),
        _ => read_atom(tokens)
    }
}

fn ignore_comment(tokens: &mut TokenStream) -> MalRet {
    tokens.pop();
    read_form(tokens)
}

fn read_map(tokens: &mut TokenStream) -> MalRet {

    let mut map = FnvHashMap::default();

    tokens.pop();//pop delim "{"

    while let Some(token) = tokens.peek() {
        if token == "}" { break }
        let key = match read_form(tokens)? {
            MalType::String(k) 
            | MalType::Keyword(k) => (*k).clone(),
            _ => return error_msg!(format!("keys of the map must be a string or keyword!"))
        };
        if tokens.peek().is_none() {
            return error_msg!((format!("no value for key: {}", key)))
        }
        map.insert(key, read_form(tokens)?);
    }

    tokens.pop(); //pop delim "}"

    Ok(MalType::HashMap(Rc::new(map)))
}

fn read_sequence(tokens: &mut TokenStream, delimiter: &str) -> MalRet {
    tokens.pop(); //pop delim 
    let mut seq = vec![];
    while let Some(cur_token) = tokens.peek(){
        if cur_token == delimiter { break; }
        seq.push(read_form(tokens)?);
    }
    match tokens.peek() {
        Some(_) => { 
            tokens.pop(); //pop delim 
            if delimiter == ")" {
                Ok(MalType::List(Rc::new(seq)))
            } else {
                Ok(MalType::Vector(Rc::new(seq)))
            }
        }
        None => error_msg!(format!("unbalanced sequence expected: {}", delimiter))
    }
}

fn read_atom(tokens: &mut TokenStream) -> MalRet{
    let mut token = tokens.next()
        .ok_or(MalError::Message("expected a symbol".to_string()))?;

    assert!(!token.is_empty(), "empty token!");

    if is_nil(&token) {
        return Ok(MalType::Nil)
    }
    if is_boolean(&token) {
        return Ok(MalType::Bool(token.parse::<bool>().unwrap()))
    }
    if let Some(int) = is_integer(&token) {
        return Ok(MalType::Number(int))
    }
    if is_keyword(&token) {
        token.remove(0);
        token.insert(0, '\u{29E}');
        return Ok(MalType::Keyword(Rc::new(token)))
    }
    read_symbol(token, tokens)
}

fn is_integer(token: &String) -> Option<i64> {
    token.parse::<i64>().ok()
}

fn is_boolean(token: &String) -> bool {
    token == "true" || token == "false"
}

fn is_nil(token: &String) -> bool {
    token == "nil"
}

fn is_keyword(token: &String) -> bool {
    token.chars().nth(0).unwrap() == ':'
}

fn read_string(tokens: &mut TokenStream) -> MalRet {

    let mut token = tokens.next().unwrap();
    token.remove(0);

    if !token.ends_with('"') {
        return error_msg!("invalid string literal: must end with '\"' ");
    }

    let mut it = token.char_indices();
    let mut mal_str = String::with_capacity(token.len());

    while let Some((index, el)) = it.next() {
        if index == token.len()-1 && el == '"' {
            return Ok(MalType::String(Rc::new(mal_str)));
        }
        if el == '\\'  {
            let following = it.nth(0).unwrap().1;
            if following == 'n'{
                mal_str.push('\n');
            } else {
                mal_str.push(following);
            }
        } else {
            mal_str.push(el);
        }
    }
    error_msg!("invalid string literal")
}

fn tokenize(input: String) -> VecDeque<String>{
    lazy_static! {
        static ref RE: Regex = 
         Regex::new(r#"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"#)
         .unwrap();
    }
    let mut tokens = VecDeque::<String>::new();
    for cap in RE.captures_iter(&input) {
        tokens.push_back(cap[1].to_owned());
    }
    return tokens;
}

fn read_symbol(current: String, tokens: &mut TokenStream) -> MalRet {
    match  current.as_str() {
        "~@" => Ok(list![symbol!("splice-unquote"), read_form(tokens)?]),
        "@"  => Ok(list![symbol!("deref"), read_form(tokens)?]),
        "~"  => Ok(list![symbol!("unquote"), read_form(tokens)?]),
        "'"  => Ok(list![symbol!("quote"), read_form(tokens)?]),
        "`"  => Ok(list![symbol!("quasiquote"), read_form(tokens)?]),
        "^"  => {
            let val = read_form(tokens)?;
            let meta = read_form(tokens)?;
            Ok(list![symbol!("with-meta"), meta, val])
        },
        _    => Ok(symbol!(current))
    }
}
