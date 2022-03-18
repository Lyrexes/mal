use std::collections::VecDeque;
use lazy_static::lazy_static;
use regex::Regex;


pub struct Reader {
    tokens: VecDeque<String>, 
}

impl Reader {
    fn new(input: String) -> Result<Reader, String> {
        Ok(Reader { tokens: tokenize(input)? })
    }

    fn next(&mut self) -> Option<String> {
        self.tokens.pop_front()
    }

    fn peek(&mut self) -> Option<&String> {
        self.tokens.front()
    }
    
}

pub fn read_str(input: String) -> Result<(), String> {
    let mut tokens = Reader::new(input)?;

    Ok(())
}

fn tokenize(input: String) -> Result<VecDeque<String>, String> {

    lazy_static! {
        static ref RE: Regex = 
         Regex::new(r#"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"#)
         .unwrap();
    }
    unimplemented!();
}