use std::{rc::Rc, fmt};
use fnv::FnvHashMap;

#[derive(Clone)]
pub enum MalType {
    HashMap(Rc<FnvHashMap<String, MalType>>),
    Vector(Rc<Vec<MalType>>),
    List(Rc<Vec<MalType>>),
    Symbol(Rc<String>),
    String(Rc<String>),
    Keyword(Rc<String>),
    Builtin(fn(Args)->MalRet),
    Number(i64),
    Bool(bool),
    Nil
}

impl fmt::Debug for MalType {
    fn fmt<'a>(&'a self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MalType::Builtin(b) => {
                f.debug_tuple("MalType")
                 .field(b as &fn(&'a [MalType])->MalRet)
                 .finish()
            },
            _ => f.debug_tuple("MalType")
                 .field(self)
                 .finish()
        }
    }
}

impl MalType {
    pub fn apply(&self, args: Args) -> MalRet {
        match self {
            MalType::Builtin(b) => b(args),
            _ => Err("EOF: Only can apply function types!".to_string())
        }
    }
}

pub type MalError = String;
pub type MalRet   = Result<MalType, MalError>;
pub type Args<'a> = &'a[MalType];

#[macro_export]
macro_rules! symbol {
    ($sym:expr) => { MalType::Symbol(Rc::new($sym.to_string())) }
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
        let map = std::collections::HashMap::<String, Maltype>
         ::with_capacity_and_hasher(
            size,
            fnv::FnvBuildHasher::default()
        );
        $(
            map.insert($key, $val)
        )*
        map
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
