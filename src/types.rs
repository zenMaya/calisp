use std::cell::RefCell;
use std::rc::Rc;
use fnv::FnvHashMap;
use itertools::Itertools;

use crate::env::{env_bind, Env};
use crate::types::CalispErr::{ErrCalispVal, ErrString};
use crate::types::CalispVal::{Atom, Bool, Func, Hash, Int, List, CalispFunc, Nil, Str, Sym, Vector};

#[derive(Debug, Clone)]
pub enum CalispVal {
    Nil,
    Bool(bool),
    Int(i64),
    Str(String),
    Sym(String),
    List(Rc<Vec<CalispVal>>, Rc<CalispVal>),
    Vector(Rc<Vec<CalispVal>>, Rc<CalispVal>),
    Hash(Rc<FnvHashMap<String, CalispVal>>, Rc<CalispVal>),
    Func(fn(CalispArgs) -> CalispRet, Rc<CalispVal>),
    CalispFunc {
        eval: fn(ast: CalispVal, env: Env) -> CalispRet,
        ast: Rc<CalispVal>,
        env: Env,
        params: Rc<CalispVal>,
        is_macro: bool,
        meta: Rc<CalispVal>,
    },
    Atom(Rc<RefCell<CalispVal>>),
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
        List(Rc::new($seq),Rc::new(Nil))
    }};
    [$($args:expr),*] => {{
        let v: Vec<CalispVal> = vec![$($args),*];
        List(Rc::new(v),Rc::new(Nil))
    }}
}

macro_rules! vector {
  ($seq:expr) => {{
    Vector(Rc::new($seq),Rc::new(Nil))
  }};
  [$($args:expr),*] => {{
    let v: Vec<CalispVal> = vec![$($args),*];
    Vector(Rc::new(v),Rc::new(Nil))
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
    Atom(Rc::new(RefCell::new(cv.clone())))
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
            List(l, _) | Vector(l, _) => Ok(Bool(l.len() == 0)),
            Nil => Ok(Bool(true)),
            _ => error("invalid type for empty?"),
        }
    }

    pub fn count(&self) -> CalispRet {
        match self {
            List(l, _) | Vector(l, _) => Ok(Int(l.len() as i64)),
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
                let fn_env = env_bind(Some(env.clone()), p.clone(), args)?;
                Ok(eval(a.clone(), fn_env)?)
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
            Atom(a) => Ok(a.borrow().clone()),
            _ => error("attempt to deref a non-Atom"),
        }
    }

    pub fn reset_bang(&self, new: &CalispVal) -> CalispRet {
        match self {
            Atom(a) => {
                *a.borrow_mut() = new.clone();
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
                fargs.insert(0, a.borrow().clone());
                *a.borrow_mut() = f.apply(fargs)?;
                Ok(a.borrow().clone())
            }
            _ => error("attempt to swap! a non-Atom"),
        }
    }

    pub fn get_meta(&self) -> CalispRet {
        match self {
            List(_, meta) | Vector(_, meta) | Hash(_, meta) => Ok((&**meta).clone()),
            Func(_, meta) => Ok((&**meta).clone()),
            CalispFunc { meta, .. } => Ok((&**meta).clone()),
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
                *meta = Rc::new((&*new_meta).clone());
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
            (List(ref a, _), List(ref b, _))
            | (Vector(ref a, _), Vector(ref b, _))
            | (List(ref a, _), Vector(ref b, _))
            | (Vector(ref a, _), List(ref b, _)) => a == b,
            (Hash(ref a, _), Hash(ref b, _)) => a == b,
            (CalispFunc { .. }, CalispFunc { .. }) => false,
            _ => false,
        }
    }
}

pub fn func(f: fn(CalispArgs) -> CalispRet) -> CalispVal {
    Func(f, Rc::new(Nil))
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
    Ok(Hash(Rc::new(hm), Rc::new(Nil)))
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
    Ok(Hash(Rc::new(hm), Rc::new(Nil)))
}

pub fn hash_map(kvs: CalispArgs) -> CalispRet {
    let hm: FnvHashMap<String, CalispVal> = FnvHashMap::default();
    _assoc(hm, kvs)
}
