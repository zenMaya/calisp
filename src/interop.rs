use crate::types::CalispVal::{Func, Nil, Str};
use crate::types::{error, CalispArgs, CalispRet, CalispVal};

use std::collections::HashMap;
use std::sync::{Arc, RwLock};

lazy_static! {
  static ref VARIABLES: Arc<RwLock<HashMap<String, CalispVal>>> =
    Arc::new(RwLock::new(HashMap::default()));
  static ref FUNCTIONS: Arc<RwLock<HashMap<String, CalispVal>>> =
    Arc::new(RwLock::new(HashMap::default()));
}

pub struct CalispInterop {}

impl CalispInterop {
  /// Register rust function that can be called from within Calisp
  pub fn register_fn(f_name: String, f: fn(CalispArgs) -> CalispRet) {
    FUNCTIONS
      .write()
      .unwrap()
      .insert(f_name, Func(f, Arc::new(RwLock::new(Nil))));
  }

  pub(crate) fn eval_fn(a: CalispArgs) -> CalispRet {
    match &a[0] {
      Str(f_name) => match FUNCTIONS.read().unwrap().get(&f_name.clone()) {
        Some(function) => Ok(function.clone()),
        _ => error("rust-fn not found"),
      },
      _ => error("rust-fn name not string"),
    }
  }

  /// Register rust variable that can be set from within Calisp
  pub fn register_var(v_name: String, v: CalispVal) {
    VARIABLES.write().unwrap().insert(v_name, v);
  }

  pub(crate) fn set_var(a: CalispArgs) -> CalispRet {
    match &a[0] {
        Str(v_name) => match VARIABLES.write().unwrap().insert(v_name.to_string(), a[1].clone()) {
            None => error("rust-var not found"),
            Some(var) => Ok(var)
        },
        _ => error("rust-var name not string"),
    }
  }

    pub (crate) fn get_var(a: CalispArgs) -> CalispRet {
         match &a[0] {
             Str(v_name) => match VARIABLES.read().unwrap().get(&v_name.clone()) {
                 Some(variable) => Ok(variable.clone()),
                 _ => error("rust-var not found"),
             },
             _ => error("rust-var name not string"),
         }
    }    
}

