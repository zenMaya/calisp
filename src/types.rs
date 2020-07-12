use fnv::FnvHashMap;
use itertools::Itertools;
use std::sync::{Arc, RwLock};

use crate::env::{env_bind, Env};
use crate::types::CalispErr::{ErrCalispVal, ErrString};
use crate::types::CalispVal::{
    Atom, Bool, CalispFunc, Func, Hash, Int, List, Nil, Str, Sym, Vector,
};

#[derive(Debug, Clone)]
pub enum CalispVal {
    Nil,
    Bool(bool),
    Int(i64),
    Str(String),
    Sym(String),
    List(Arc<RwLock<Vec<CalispVal>>>, Arc<RwLock<CalispVal>>),
    Vector(Arc<RwLock<Vec<CalispVal>>>, Arc<RwLock<CalispVal>>),
    Hash(Arc<RwLock<FnvHashMap<String, CalispVal>>>, Arc<RwLock<CalispVal>>),
    Func(fn(CalispArgs) -> CalispRet, Arc<RwLock<CalispVal>>),
    CalispFunc {
        eval: fn(ast: CalispVal, env: Env) -> CalispRet,
        ast: Arc<RwLock<CalispVal>>,
        env: Env,
        params: Arc<RwLock<CalispVal>>,
        is_macro: bool,
        meta: Arc<RwLock<CalispVal>>,
    },
    Atom(Arc<RwLock<CalispVal>>),
}

#[derive(Debug)]
pub enum CalispErr {
    ErrString(String),
    ErrCalispVal(CalispVal),
}

pub type CalispArgs = Vec<CalispVal>;
pub type CalispRet = Result<CalispVal, CalispErr>;

macro_rules! list {
    ($seq:expr) => {{
        List(std::sync::Arc::new(std::sync::RwLock::new($seq)),std::sync::Arc::new(std::sync::RwLock::new(Nil)))
    }};
    [$($args:expr),*] => {{
        let v: Vec<CalispVal> = vec![$($args),*];
        List(std::sync::Arc::new(std::sync::RwLock::new(v)),std::sync::Arc::new(std::sync::RwLock::new(Nil)))
    }}
}

macro_rules! vector {
  ($seq:expr) => {{
    Vector(std::sync::Arc::new(std::sync::RwLock::new($seq)),std::sync::Arc::new(std::sync::RwLock::new(Nil)))
  }};
  [$($args:expr),*] => {{
    let v: Vec<CalispVal> = vec![$($args),*];
    Vector(std::sync::Arc::new(std::sync::RwLock::new(v)),std::sync::Arc::new(std::sync::RwLock::new(Nil)))
  }}
}

pub fn error(s: &str) -> CalispRet {
    Err(ErrString(s.to_string()))
}

pub fn format_error(e: CalispErr) -> String {
    match e {
        ErrString(s) => s.clone(),
        ErrCalispVal(cv) => cv.pr_str(true),
    }
}

pub fn atom(cv: &CalispVal) -> CalispVal {
    Atom(Arc::new(RwLock::new(cv.clone())))
}

impl CalispVal {
    pub fn keyword(&self) -> CalispRet {
        match self {
            Str(s) if s.starts_with("\u{29e}") => Ok(Str(s.to_string())),
            Str(s) => Ok(Str(format!("\u{29e}{}", s))),
            _ => error("invalid type for keyword"),
        }
    }

    pub fn empty_q(&self) -> CalispRet {
        match self {
            List(l, _) | Vector(l, _) => Ok(Bool(l.read().unwrap().len() == 0)),
            Nil => Ok(Bool(true)),
            _ => error("invalid type for empty?"),
        }
    }

    pub fn count(&self) -> CalispRet {
        match self {
            List(l, _) | Vector(l, _) => Ok(Int(l.read().unwrap().len() as i64)),
            Nil => Ok(Int(0)),
            _ => error("invalid type for count"),
        }
    }

    pub fn apply(&self, args: CalispArgs) -> CalispRet {
        match *self {
            Func(f, _) => f(args),
            CalispFunc {
                eval,
                ref ast,
                ref env,
                ref params,
                ..
            } => {
                let a = &**ast;
                let p = &**params;
                let fn_env = env_bind(Some(env.clone()), p.read().unwrap().clone(), args)?;
                Ok(eval(a.read().unwrap().clone(), fn_env)?)
            }
            _ => error("attempt to call non-function"),
        }
    }

    pub fn keyword_q(&self) -> bool {
        match self {
            Str(s) if s.starts_with("\u{29e}") => true,
            _ => false,
        }
    }

    pub fn deref(&self) -> CalispRet {
        match self {
            Atom(a) => Ok(a.read().unwrap().clone()),
            _ => error("attempt to deref a non-Atom"),
        }
    }

    pub fn reset_bang(&self, new: &CalispVal) -> CalispRet {
        match self {
            Atom(a) => {
                *a.write().unwrap() = new.clone();
                Ok(new.clone())
            }
            _ => error("attempt to reset! a non-Atom"),
        }
    }

    pub fn swap_bang(&self, args: &CalispArgs) -> CalispRet {
        match self {
            Atom(a) => {
                let f = &args[0];
                let mut fargs = args[1..].to_vec();
                fargs.insert(0, a.read().unwrap().clone());
                *a.write().unwrap() = f.apply(fargs)?;
                drop(a);
                Ok(a.read().unwrap().clone())
            }
            _ => error("attempt to swap! a non-Atom"),
        }
    }

    pub fn get_meta(&self) -> CalispRet {
        match self {
            List(_, meta) | Vector(_, meta) | Hash(_, meta) => Ok((&**meta).read().unwrap().clone()),
            Func(_, meta) => Ok((&**meta).read().unwrap().clone()),
            CalispFunc { meta, .. } => Ok((&**meta).read().unwrap().clone()),
            _ => error("meta not supported by type"),
        }
    }

    pub fn with_meta(&mut self, new_meta: &CalispVal) -> CalispRet {
        match self {
            List(_, ref mut meta)
            | Vector(_, ref mut meta)
            | Hash(_, ref mut meta)
            | Func(_, ref mut meta)
            | CalispFunc { ref mut meta, .. } => {
                *meta = Arc::new(RwLock::new((&*new_meta).clone()));
            }
            _ => return error("with-meta not supported by type"),
        };
        Ok(self.clone())
    }
}

impl PartialEq for CalispVal {
    fn eq(&self, other: &CalispVal) -> bool {
        match (self, other) {
            (Nil, Nil) => true,
            (Bool(ref a), Bool(ref b)) => a == b,
            (Int(ref a), Int(ref b)) => a == b,
            (Str(ref a), Str(ref b)) => a == b,
            (Sym(ref a), Sym(ref b)) => a == b,
            (List(a, _), List(b, _))
            | (Vector( a, _), Vector( b, _))
            | (List( a, _), Vector( b, _))
            | (Vector( a, _), List( b, _)) => *a.read().unwrap() == *b.read().unwrap(),
            (Hash(a, _), Hash(b, _)) => *a.read().unwrap() == *b.read().unwrap(),
            (CalispFunc { .. }, CalispFunc { .. }) => false,
            _ => false,
        }
    }
}

pub fn func(f: fn(CalispArgs) -> CalispRet) -> CalispVal {
    Func(f, Arc::new(RwLock::new(Nil)))
}

pub fn _assoc(mut hm: FnvHashMap<String, CalispVal>, kvs: CalispArgs) -> CalispRet {
    if kvs.len() % 2 != 0 {
        return error("odd number of elements");
    }
    for (k, v) in kvs.iter().tuples() {
        match k {
            Str(s) => {
                hm.insert(s.to_string(), v.clone());
            }
            _ => return error("key is not string"),
        }
    }
    Ok(Hash(Arc::new(RwLock::new(hm)), Arc::new(RwLock::new(Nil))))
}

pub fn _dissoc(mut hm: FnvHashMap<String, CalispVal>, ks: CalispArgs) -> CalispRet {
    for k in ks.iter() {
        match k {
            Str(ref s) => {
                hm.remove(s);
            }
            _ => return error("key is not string"),
        }
    }
    Ok(Hash(Arc::new(RwLock::new(hm)), Arc::new(RwLock::new(Nil))))
}

pub fn hash_map(kvs: CalispArgs) -> CalispRet {
    let hm: FnvHashMap<String, CalispVal> = FnvHashMap::default();
    _assoc(hm, kvs)
}
