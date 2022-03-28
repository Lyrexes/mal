use fnv::FnvHashMap;
use std::cell::RefCell;
use std::rc::{Rc};
use crate::types::{MalType, MalType::{Symbol}};

pub trait EnvInterface {
    fn new_env(outer: Option<Env>) -> Env;
    fn with_binds_mal(outer: Option<Env>, binds: &[MalType], exprs: &[MalType])
     -> Result<Env, String>;
    fn with_binds(outer: Option<Env>, binds: &[Rc<String>], exprs: &[MalType]) -> Env;
    fn with_map(outer: Option<Env>, talbe: FnvHashMap::<String, MalType>) -> Env;
    fn insert_mal(&mut self, binds: &[MalType], exprs: &[MalType])
     -> Result<(), String>;
    fn insert_fast(&mut self, binds: &[Rc<String>], exprs: &[MalType]);
    fn insert(&mut self, binds: &[Rc<String>], exprs: &[MalType]);
    fn reserve(&mut self, additional: usize);
    fn set(&mut self, key: String, value: MalType);
    fn find(&self, key: &String) -> Option<Env>;
    fn get(&self, key: &String) -> Option<MalType>;
}
pub struct EnvInner {
    table: RefCell<FnvHashMap::<String, MalType>>,
    outer: Option<Env>
}
pub type Env = Rc<EnvInner>;

impl EnvInterface for Env {
    fn new_env(outer: Option<Env>) -> Env {
        Rc::new(EnvInner {
            table: RefCell::new(FnvHashMap::<String, MalType>::default()),
            outer
        })
    }

    fn reserve(&mut self, additional: usize) {
        self.table
         .borrow_mut()
         .reserve(additional)
    }

    fn insert(&mut self, binds: &[Rc<String>], exprs: &[MalType]){
        assert_eq!(binds.len(), exprs.len());
        for index in 0..binds.len() {
            self.table
             .borrow_mut()
             .insert((*binds[index]).clone(), exprs[index].clone());
        }
    }

    fn with_binds(outer: Option<Env>, binds: &[Rc<String>], exprs: &[MalType]) -> Self {
        let mut _self = Rc::new(EnvInner {
            table: RefCell::new(FnvHashMap::<String, MalType>::default()),
            outer
         });
        _self.insert_fast(binds, exprs);
        _self
    }

    fn with_binds_mal(outer: Option<Env>, binds: &[MalType], exprs: &[MalType]) -> Result<Env, String> {
        let mut _self = Rc::new(EnvInner {
            table: RefCell::new(FnvHashMap::<String, MalType>::default()),
            outer
         });
        _self.insert_mal(binds, exprs)?;
        Ok(_self)
    }

    fn with_map(outer: Option<Env>, table:FnvHashMap::<String, MalType>) ->Env{
        Rc::new(EnvInner{ table: RefCell::new(table) , outer })
    }

    fn insert_fast(&mut self, binds: &[Rc<String>], exprs: &[MalType]) {
        assert_eq!(binds.len(), exprs.len());
        self.table.borrow_mut().reserve(binds.len());
        for index in 0..binds.len() {
            self.table
             .borrow_mut()
             .insert((*binds[index]).clone(), exprs[index].clone());
        }
    }

    fn insert_mal(&mut self, binds: &[MalType], exprs: &[MalType]) -> Result<(), String>{
        assert_eq!(binds.len(), exprs.len());
        self.table.borrow_mut().reserve(binds.len());
        for index in 0..binds.len() {
            match binds[index] {
                Symbol(ref key) => {
                    self.table
                     .borrow_mut()
                     .insert((**key).clone(), exprs[index].clone());
                },
                _ => return Err(
                    "expected Symbol in bindings!"
                    .to_owned()
                )
            }
        }
        Ok(())
    }
    fn set(&mut self, key: String, value: MalType) {
        self.table
         .borrow_mut()
         .insert(key, value);
    }

    fn find(&self, key: &String) -> Option<Env> {
        if self.table.borrow().contains_key(key) {
            return Some(self.clone());
        }
        if let Some(outer) = self.outer.clone() {
            return outer.find(key);
        }
        None
    }

    fn get(&self, key: &String) -> Option<MalType> {
        if let Some(env) = self.find(key) {
            if Rc::ptr_eq(self, &env) {
                return Some(self.table.borrow().get(key)?.clone());
            }
            else {
                return env.get(key);
            }
        }
        None
    }

}