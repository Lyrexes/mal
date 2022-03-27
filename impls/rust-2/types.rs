use std::{rc::Rc, fmt, cell::RefCell};
use fnv::FnvHashMap;
use crate::env::Env;


#[derive(Clone)]
pub enum MalType {
    HashMap(Rc<FnvHashMap<String, MalType>>),
    Vector(Rc<Vec<MalType>>),
    List(Rc<Vec<MalType>>),
    Symbol(Rc<String>),
    String(Rc<String>),
    Keyword(Rc<String>),
    Builtin(fn(Args)->MalRet),
    Lambda(Rc<dyn Fn(Args)->Result<(MalType, Env),MalError>>),
    Atom(Rc<RefCell<MalType>>),
    Number(i64),
    Bool(bool),
    Nil
}

#[macro_export]
macro_rules! symbol {
    ($sym:expr) => { MalType::Symbol(Rc::new($sym.to_string())) }
}

#[macro_export]
macro_rules! keyword {
    ($sym:expr) => { MalType::Keyword(Rc::new($sym.to_string())) }
}

#[macro_export]
macro_rules! atom {
    ($atom:expr) => { MalType::Atom(Rc::new(RefCell::new($atom))) }
}

#[macro_export]
macro_rules! string {
    ($string:expr) => { MalType::String(Rc::new($string.to_string())) }
}

#[macro_export]
macro_rules! vector {
    ($el:expr) => {
        MalType::Vector(Rc::new($el))
    };
    ($($el:expr),*) => {{
        let vec:Vec<MalType> = vec![$($el),*];
        MalType::Vector(Rc::new(vec))
    }}
}

#[macro_export]
macro_rules! count {
    () => {0usize};
    ( $x:tt $($xs:tt)* ) => {1usize + count!($($xs)*)};
}

#[macro_export]
macro_rules! map {
    () => {
        MalType::HashMap(
            Rc::new(FnvHashMap::<String, MalType>::default())
        )
    };
    ($el:expr) => {
        MalType::HashMap(Rc::new($el))
    };
    ($($key:expr => $val:expr),*) => {{
        let size = count!($($key)*);
        let map = std::collections::HashMap::<String, MalType>
         ::with_capacity_and_hasher(
            size,
            fnv::FnvBuildHasher::default()
        );
        $(
            map.insert($key, $val)
        )*
        map!(map)
    }}
}

#[macro_export]
macro_rules! list {
    ($el:expr) => {
        MalType::List(Rc::new($el))
    };
    ($($el:expr),*) => {{
        let vec:Vec<MalType> = vec![$($el),*];
        MalType::List(Rc::new(vec))
    }}
}
impl PartialEq for MalType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (MalType::Nil, MalType::Nil) => true,
            (MalType::String(lhs), MalType::String(rhs)) => lhs == rhs,
            (MalType::Symbol(lhs), MalType::Symbol(rhs)) => lhs == rhs,
            (MalType::Number(lhs), MalType::Number(rhs)) => lhs == rhs,
            (MalType::Bool(lhs), MalType::Bool(rhs)) => lhs == rhs,
            (MalType::Keyword(lhs), MalType::Keyword(rhs)) => lhs == rhs,
            (MalType::HashMap(lhs), MalType::HashMap(rhs)) => lhs == rhs,
            (MalType::List(lhs) | MalType::Vector(lhs),
             MalType::List(rhs) | MalType::Vector(rhs)) => lhs == rhs,
            _ => false
        }
    }
}

impl Eq for MalType {}

impl MalType {
    pub fn apply(&self, args: Args) -> MalRet {
        match self {
            MalType::Builtin(b) => b(args),
            _ => Err("EOF: Only can apply function types!".to_string())
        }
    }

    pub fn is_form_call(&self, form: &str, args:usize) -> bool {
        match self {
            MalType::List(lst) if lst.len() == args+1 => {
                if let MalType::Symbol(ref sym) = lst[0] {
                    **sym == form
                } else {
                    false
                }
            } 
            _ => false
        }
    }
    pub fn nth(&self, i:usize) -> MalType {
        match self {
            MalType::List(lst) | MalType::Vector(lst) => {
                if lst.len() <= i {
                    panic!("nth: index out of bounds!")
                } else {
                    lst[i].clone()
               }
            }
            _=> panic!("nth can only be used with lists or vectors!")
        }
    }

    pub fn deep_copy(&self) -> MalType {
        match self {
            MalType::HashMap(m) => MalType::HashMap(Rc::new((**m).clone())),
            MalType::Vector(v) =>  vector!((**v).clone()),
            MalType::List(l) => list!((**l).clone()),
            MalType::Symbol(s) => symbol!((**s).clone()),
            MalType::String(s) => string!((**s).clone()),
            MalType::Keyword(s) => keyword!((**s).clone()),
            MalType::Builtin(_) | MalType::Lambda(_) => self.clone(),
            MalType::Atom(a) => atom!((**a).borrow().deep_copy()),
            MalType::Number(n) => MalType::Number(*n),
            MalType::Bool(b) => MalType::Bool(*b),
            MalType::Nil => self.clone(),
        }
    }
}

pub type MalError = String;
pub type MalRet   = Result<MalType, MalError>;
pub type Args<'a> = &'a[MalType];


impl fmt::Debug for MalType {
    fn fmt<'a>(&'a self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MalType::Builtin(_) | MalType::Lambda(_)  => {
                f.debug_tuple("Function")
                 .finish()
            },
            MalType::HashMap(h) => {
                f.debug_map()
                 .entries(h.iter())
                 .finish()
            },
            MalType::Vector(l)
             | MalType::List(l) => {
               f.debug_list()
                .entries(l.iter())
                .finish() 
            },
            MalType::Symbol(v) => {
                f.debug_tuple("Symbol")
                 .field(v)
                 .finish()
            }
            MalType::String(v) => {
                f.debug_tuple("String")
                 .field(v)
                 .finish()
            },
            MalType::Keyword(v) => {
                f.debug_tuple("Keyword")
                 .field(v)
                 .finish()
            },
            MalType::Number(v) => {
                f.debug_tuple("Number")
                 .field(v)
                 .finish()
            },
            MalType::Bool(v) => {
                f.debug_tuple("Bool")
                 .field(v)
                 .finish()
            }
            MalType::Atom(v) => {
                f.debug_tuple("Atom")
                 .field(v)
                 .finish()
            },
            MalType::Nil => f.debug_tuple("Nil").finish(),
        }
    }
}