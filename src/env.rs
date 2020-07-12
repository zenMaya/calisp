use fnv::FnvHashMap;
use std::sync::{Arc, RwLock};

use crate::types::CalispErr::ErrString;
use crate::types::CalispVal::{List, Nil, Sym, Vector};
use crate::types::{error, CalispErr, CalispRet, CalispVal};

#[derive(Debug)]
pub struct EnvStruct {
    data: Arc<RwLock<FnvHashMap<String, CalispVal>>>,
    pub outer: Option<Env>,
}

pub type Env = Arc<RwLock<EnvStruct>>;

pub fn env_new(outer: Option<Env>) -> Env {
    Arc::new(
        RwLock::new(EnvStruct {
            data: Arc::new(RwLock::new(FnvHashMap::default())),
            outer: outer,
    }))
}

pub fn env_bind(
    outer: Option<Env>,
    mbinds: CalispVal,
    exprs: Vec<CalispVal>,
) -> Result<Env, CalispErr> {
    let env = env_new(outer);
    match mbinds {
        List(binds, _) | Vector(binds, _) => {
            for (i, b) in binds.read().unwrap().iter().enumerate() {
                match b {
                    Sym(s) if s == "&" => {
                        env_set(&env, binds.read().unwrap()[i + 1].clone(), list!(exprs[i..].to_vec()))?;
                        break;
                    }
                    _ => {
                        env_set(&env, b.clone(), exprs[i].clone())?;
                    }
                }
            }
            Ok(env)
        }
        _ => Err(ErrString("env_bind binds not List/Vector".to_string())),
    }
}

pub fn env_find(env: &Env, key: &str) -> Option<Env> {
    match (env.read().unwrap().data.read().unwrap().contains_key(key), env.read().unwrap().outer.clone()) {
        (true, _) => Some(env.clone()),
        (false, Some(o)) => env_find(&o, key),
        _ => None,
    }
}

pub fn env_get(env: &Env, key: &CalispVal) -> CalispRet {
    match key {
        Sym(ref s) => match env_find(env, s) {
            Some(e) => Ok(e
                          .read()
                          .unwrap()
                .data
                          .read()
                          .unwrap()
                .get(s)
                .ok_or(ErrString(format!("'{}' not found", s)))?
                .clone()),
            _ => error(&format!("'{}' not found", s)),
        },
        _ => error("Env.get called with non-Str"),
    }
}

pub fn env_set(env: &Env, key: CalispVal, val: CalispVal) -> CalispRet {
    match key {
        Sym(ref s) => {
            env.write().unwrap().data.write().unwrap().insert(s.to_string(), val.clone());
            Ok(val)
        }
        _ => error("Env.set called with non-Str"),
    }
}

pub fn env_sets(env: &Env, key: &str, val: CalispVal) {
    env.write().unwrap().data.write().unwrap().insert(key.to_string(), val);
}
