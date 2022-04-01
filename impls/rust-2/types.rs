use std::{rc::Rc, fmt, cell::RefCell};
use fnv::FnvHashMap;
use crate::env::Env;


#[derive(Clone)]
pub enum MalType {
    HashMap(Rc<FnvHashMap<String, MalType>>, Rc<MalType>),
    Vector(Rc<Vec<MalType>>, Rc<MalType>),
    List(Rc<Vec<MalType>>, Rc<MalType>),
    Symbol(Rc<String>),
    String(Rc<String>),
    Keyword(Rc<String>),
    Builtin(fn(Args)->MalRet, Rc<MalType>),
    Lambda(Rc<dyn Fn(Args)->Result<(MalType, Env),MalError>>, Rc<MalType>),
    Macro(Rc<dyn Fn(Args)->Result<(MalType, Env),MalError>>, Rc<MalType>),
    Atom(Rc<RefCell<MalType>>),
    Number(i64),
    Bool(bool),
    Nil
}

#[macro_export]
macro_rules! builtin {
    ($func:expr) => { Builtin($func, Rc::new(MalType::Nil)) }
}

#[macro_export]
macro_rules! error_msg {
    ($msg:expr) => { Err(MalError::Message($msg.to_string())) }
}

#[macro_export]
macro_rules! error_type {
    ($sym:expr) => { Err(MalError::MalVal(sym.clone())) }
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
        MalType::Vector(Rc::new($el), Rc::new(MalType::Nil))
    };
    ($($el:expr),*) => {{
        let vec:Vec<MalType> = vec![$($el),*];
        MalType::Vector(Rc::new(vec), Rc::new(MalType::Nil))
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
        MalType::HashMap(Rc::new($el), Rc::new(MalType::Nil))
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
        MalType::List(Rc::new($el), Rc::new(MalType::Nil))
    };
    ($($el:expr),*) => {{
        let vec:Vec<MalType> = vec![$($el),*];
        MalType::List(Rc::new(vec), Rc::new(MalType::Nil))
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
            (MalType::HashMap(lhs, _), MalType::HashMap(rhs,_)) => lhs == rhs,
            (MalType::List(lhs,_) | MalType::Vector(lhs,_),
             MalType::List(rhs,_) | MalType::Vector(rhs,_)) => lhs == rhs,
            _ => false
        }
    }
}

impl Eq for MalType {}

impl MalType {
    pub fn apply(&self, args: Args) -> MalRet {
        match self {
            MalType::Builtin(b,_) => b(args),
            _ => error_msg!("Only can apply function types!")
        }
    }

    pub fn is_form_call(&self, form: &str, args:usize) -> bool {
        match self {
            MalType::List(lst,_) if lst.len() == args+1 => {
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
            MalType::List(lst,_) | MalType::Vector(lst,_) => {
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
            MalType::HashMap(m, me) => MalType::HashMap(Rc::new((**m).clone()), (*me).clone()),
            MalType::Vector(v, me) =>  MalType::Vector(Rc::new((**v).clone()), (*me).clone()),
            MalType::List(l, me) => MalType::List(Rc::new((**l).clone()), (*me).clone()),
            MalType::Symbol(s) => symbol!((**s).clone()),
            MalType::String(s) => string!((**s).clone()),
            MalType::Keyword(s) => keyword!((**s).clone()),
            MalType::Builtin(_,_) | MalType::Lambda(_,_) | MalType::Macro(_,_) => self.clone(),
            MalType::Atom(a) => atom!((**a).borrow().deep_copy()),
            MalType::Number(n) => MalType::Number(*n),
            MalType::Bool(b) => MalType::Bool(*b),
            MalType::Nil => self.clone(),
        }
    }
} 

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MalError {
    MalVal(MalType),
    Message(String)
}

pub type MalRet   = Result<MalType, MalError>;
pub type Args<'a> = &'a[MalType];

impl fmt::Debug for MalType {
    fn fmt<'a>(&'a self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MalType::Builtin(_,_) |
             MalType::Lambda(_,_) | 
             MalType::Macro(_,_)  => {
                f.debug_tuple("Function")
                 .finish()
            },
            MalType::HashMap(h,_) => {
                f.debug_map()
                 .entries(h.iter())
                 .finish()
            },
            MalType::Vector(l,_)
             | MalType::List(l,_) => {
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