#![allow(non_snake_case)]
#![allow(dead_code)]
//#![recursion_limit="512"]

use std::sync::{Arc, RwLock};

use fnv::FnvHashMap;
use itertools::Itertools;

#[macro_use]
extern crate lazy_static;
extern crate fnv;
extern crate itertools;
extern crate regex;
extern crate rustyline;
use rustyline::error::ReadlineError;
use rustyline::Editor;

#[macro_use]
#[path = "types.rs"]
mod types;
use crate::types::CalispErr::{ErrCalispVal, ErrString};
use crate::types::CalispVal::{Bool, CalispFunc, Func, Hash, List, Nil, Str, Sym, Vector};
use crate::types::{error, format_error, CalispArgs, CalispErr, CalispRet, CalispVal};
#[path = "env.rs"]
mod env;
#[path = "printer.rs"]
mod printer;
#[path = "reader.rs"]
mod reader;
use crate::env::{env_bind, env_find, env_get, env_new, env_set, env_sets, Env};
#[macro_use]
#[path = "core.rs"]
mod core;
#[path = "interop.rs"]
mod interop;

/// Read function for reading plain text and parsing it into AST
fn read(str: &str) -> CalispRet {
  reader::read_str(str.to_string())
}

/// Handle quasiquotes `\``
fn quasiquote(ast: &CalispVal) -> CalispVal {
  match ast {
    List(v, _) | Vector(v, _) if v.read().unwrap().len() > 0 => {
      let a0 = &v.read().unwrap()[0];
      match a0 {
        Sym(ref s) if s == "unquote" => v.read().unwrap()[1].clone(),
        List(ref v0, _) | Vector(ref v0, _) if v0.read().unwrap().len() > 0 => {
          match v0.read().unwrap()[0] {
            Sym(ref s) if s == "splice-unquote" => list![
              Sym("concat".to_string()),
              v0.read().unwrap()[1].clone(),
              quasiquote(&list!(v.read().unwrap()[1..].to_vec()))
            ],
            _ => list![
              Sym("cons".to_string()),
              quasiquote(&a0),
              quasiquote(&list!(v.read().unwrap()[1..].to_vec()))
            ],
          }
        }
        _ => list![
          Sym("cons".to_string()),
          quasiquote(a0),
          quasiquote(&list!(v.read().unwrap()[1..].to_vec()))
        ],
      }
    }
    _ => list![Sym("quote".to_string()), ast.clone()],
  }
}

/// Check if call is macro call
fn is_macro_call(ast: &CalispVal, env: &Env) -> Option<(CalispVal, CalispArgs)> {
  match ast {
    List(v, _) => match v.read().unwrap()[0] {
      Sym(ref s) => match env_find(env, s) {
        Some(e) => match env_get(&e, &v.read().unwrap()[0]) {
          Ok(f @ CalispFunc { is_macro: true, .. }) => Some((f, v.read().unwrap()[1..].to_vec())),
          _ => None,
        },
        _ => None,
      },
      _ => None,
    },
    _ => None,
  }
}

/// Expand macro
fn macroexpand(mut ast: CalispVal, env: &Env) -> (bool, CalispRet) {
  let mut was_expanded = false;
  while let Some((mf, args)) = is_macro_call(&ast, env) {
    ast = match mf.apply(args) {
      Err(e) => return (false, Err(e)),
      Ok(a) => a,
    };
    was_expanded = true;
  }
  (was_expanded, Ok(ast))
}

/// Evaluate AST
fn eval_ast(ast: &CalispVal, env: &Env) -> CalispRet {
  match ast {
    Sym(_) => Ok(env_get(&env, &ast)?),
    List(v, _) => {
      let mut lst: CalispArgs = vec![];
      for a in v.read().unwrap().iter() {
        lst.push(eval(a.clone(), env.clone())?)
      }
      Ok(list!(lst))
    }
    Vector(v, _) => {
      let mut lst: CalispArgs = vec![];
      for a in v.read().unwrap().iter() {
        lst.push(eval(a.clone(), env.clone())?)
      }
      Ok(vector!(lst))
    }
    Hash(hm, _) => {
      let mut new_hm: FnvHashMap<String, CalispVal> = FnvHashMap::default();
      for (k, v) in hm.write().unwrap().iter() {
        new_hm.insert(k.to_string(), eval(v.clone(), env.clone())?);
      }
      Ok(Hash(
        Arc::new(RwLock::new(new_hm)),
        Arc::new(RwLock::new(Nil)),
      ))
    }
    _ => Ok(ast.clone()),
  }
}

/// Evaluate
/// This function loops through the whole AST and evaluates every expression accordingly
fn eval(mut ast: CalispVal, mut env: Env) -> CalispRet {
  let ret: CalispRet;

  'tco: loop {
    ret = match ast.clone() {
      List(l, _) => {
        if l.read().unwrap().len() == 0 {
          return Ok(ast);
        }
        match macroexpand(ast.clone(), &env) {
          (true, Ok(new_ast)) => {
            ast = new_ast;
            continue 'tco;
          }
          (_, Err(e)) => return Err(e),
          _ => (),
        }

        if l.read().unwrap().len() == 0 {
          return Ok(ast);
        }
        let a0 = &l.read().unwrap()[0];
        match a0 {
          Sym(ref a0sym) if a0sym == "def!" => env_set(
            &env,
            l.read().unwrap()[1].clone(),
            eval(l.read().unwrap()[2].clone(), env.clone())?,
          ),
          Sym(ref a0sym) if a0sym == "let*" => {
            env = env_new(Some(env.clone()));
            let (a1, a2) = (l.read().unwrap()[1].clone(), l.read().unwrap()[2].clone());
            match a1 {
              List(ref binds, _) | Vector(ref binds, _) => {
                for (b, e) in binds.read().unwrap().iter().tuples() {
                  match b {
                    Sym(_) => {
                      let _ = env_set(&env, b.clone(), eval(e.clone(), env.clone())?);
                    }
                    _ => {
                      return error("let* with non-Sym binding");
                    }
                  }
                }
              }
              _ => {
                return error("let* with non-List bindings");
              }
            };
            ast = a2;
            continue 'tco;
          }
          Sym(ref a0sym) if a0sym == "quote" => Ok(l.read().unwrap()[1].clone()),
          Sym(ref a0sym) if a0sym == "quasiquote" => {
            ast = quasiquote(&l.read().unwrap()[1]);
            continue 'tco;
          }
          Sym(ref a0sym) if a0sym == "defmacro!" => {
            let (a1, a2) = (l.read().unwrap()[1].clone(), l.read().unwrap()[2].clone());
            let mut docstring = String::default();
            let a3 = match a2 {
              Str(s) => {
                docstring = s;
                l.read().unwrap()[3].clone()
              }
              _ => a2,
            };
            let r = eval(a3, env.clone())?;
            match r {
              CalispFunc {
                eval,
                ast,
                env,
                params,
                ..
              } => Ok(env_set(
                &env,
                a1.clone(),
                CalispFunc {
                  eval: eval,
                  ast: ast.clone(),
                  env: env.clone(),
                  params: params.clone(),
                  is_macro: true,
                  meta: Arc::new(RwLock::new(Nil)),
                  docstring: docstring,
                },
              )?),
              _ => error("set_macro on non-function"),
            }
          }
          Sym(ref a0sym) if a0sym == "macroexpand" => {
            match macroexpand(l.read().unwrap()[1].clone(), &env) {
              (_, Ok(new_ast)) => Ok(new_ast),
              (_, e) => return e,
            }
          }
          Sym(ref a0sym) if a0sym == "try*" => {
            match eval(l.read().unwrap()[1].clone(), env.clone()) {
              Err(ref e) if l.read().unwrap().len() >= 3 => {
                let exc = match e {
                  ErrCalispVal(cv) => cv.clone(),
                  ErrString(s) => Str(s.to_string()),
                };
                match l.read().unwrap()[2].clone() {
                  List(c, _) => {
                    let catch_env = env_bind(
                      Some(env.clone()),
                      list!(vec![c.read().unwrap()[1].clone()]),
                      vec![exc],
                    )?;
                    eval(c.read().unwrap()[2].clone(), catch_env)
                  }
                  _ => error("invalid catch block"),
                }
              }
              res => res,
            }
          }
          Sym(ref a0sym) if a0sym == "do" => {
            match eval_ast(
              &list!(l.read().unwrap()[1..l.read().unwrap().len() - 1].to_vec()),
              &env,
            )? {
              List(_, _) => {
                ast = l.read().unwrap().last().unwrap_or(&Nil).clone();
                continue 'tco;
              }
              _ => error("invalid do form"),
            }
          }
          Sym(ref a0sym) if a0sym == "if" => {
            let cond = eval(l.read().unwrap()[1].clone(), env.clone())?;
            match cond {
              Bool(false) | Nil if l.read().unwrap().len() >= 4 => {
                ast = l.read().unwrap()[3].clone();
                continue 'tco;
              }
              Bool(false) | Nil => Ok(Nil),
              _ if l.read().unwrap().len() >= 3 => {
                ast = l.read().unwrap()[2].clone();
                continue 'tco;
              }
              _ => Ok(Nil),
            }
          }
          Sym(ref a0sym) if a0sym == "fn*" => {
            let (a1, a2) = (l.read().unwrap()[1].clone(), l.read().unwrap()[2].clone());
            let mut docstring = String::default();
            let a3 = match a2 {
              Str(s) => {
                docstring = s;
                l.read().unwrap()[3].clone()
              }
              _ => a2,
            };
            Ok(CalispFunc {
              eval: eval,
              ast: Arc::new(RwLock::new(a3)),
              env: env,
              params: Arc::new(RwLock::new(a1)),
              is_macro: false,
              meta: Arc::new(RwLock::new(Nil)),
              docstring: docstring,
            })
          }
          Sym(ref a0sym) if a0sym == "eval" => {
            ast = eval(l.read().unwrap()[1].clone(), env.clone())?;
            while let Some(ref e) = env.clone().read().unwrap().outer {
              env = e.clone();
            }
            continue 'tco;
          }
          _ => match eval_ast(&ast, &env)? {
            List(ref el, _) => {
              let ref f = el.read().unwrap()[0].clone();
              let args = el.read().unwrap()[1..].to_vec();
              match f {
                Func(_, _, _) => f.apply(args),
                CalispFunc {
                  ast: mast,
                  env: menv,
                  params,
                  ..
                } => {
                  let a = &**mast;
                  let p = &**params;
                  env = env_bind(Some(menv.clone()), p.read().unwrap().clone(), args)?;
                  ast = a.read().unwrap().clone();
                  continue 'tco;
                }
                _ => error("attempt to call non-function"),
              }
            }
            _ => error("expected a list"),
          },
        }
      }
      _ => eval_ast(&ast, &env),
    };
    break;
  }

  ret
}

/// Print the result from output AST
fn print(ast: &CalispVal) -> String {
  ast.pr_str(true)
}

/// This function is called when text needs to be evaluated
/// Calls [`read`]
/// Then [`eval`]
/// And lastly [`print`]
fn rep(str: &str, env: &Env) -> Result<String, CalispErr> {
  let ast = read(str)?;
  let exp = eval(ast, env.clone())?;

  Ok(print(&exp))
}

/// Struct for handling the execution of Calisp code
/// If you want to use this as an library, use [`new`] and [`run`] functions
#[doc(inline)]
pub struct CalispInterpreter {
  input_file: String,
  repl_env: Env,
}

impl CalispInterpreter {
  #[doc(inline)]
  /// Create new instance of [`CalispInterpreter`]
  pub fn new(input_file: String, arguments: &Vec<std::string::String>) -> CalispInterpreter {
    CalispInterpreter {
      input_file: input_file,
      repl_env: CalispInterpreter::new_env(arguments.to_vec()),
    }
  }

  #[doc(inline)]
  /// Create new environment for Calisp
  fn new_env(arguments: Vec<String>) -> Env {
    let repl_env = env_new(None);
    for (k, v) in core::ns() {
      env_sets(&repl_env, k, v);
    }

    env_sets(
      &repl_env,
      "*ARGV*",
      list!(arguments.iter().map(|arg| Str(arg.to_string())).collect()),
    );

    let _ = rep("(def! *host-language* \"rust\")", &repl_env);
    let _ = rep("(def! not (fn* (a) (if a false true)))", &repl_env);
    let _ = rep(
      "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))",
      &repl_env,
    );
    let _ = rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))", &repl_env);

    repl_env.clone()
  }

  #[doc(inline)]
  /// Load file in Calisp and eval it
  pub fn run(&self) -> Result<String, CalispErr> {
    match rep(
      &format!("(load-file \"{}\")", self.input_file),
      &self.repl_env,
    ) {
      Ok(_) => std::process::exit(0),
      Err(e) => {
        println!("Error: {}", format_error(e));
        std::process::exit(1);
      }
    }
  }

  #[doc(inline)]
  /// Read code from the stdin interactively
  pub fn run_interactive(&self) {
    println!("running interactive");
    let mut rl = Editor::<()>::new();

    if rl.load_history(".calisp-history").is_err() {
      eprintln!("No previous history.");
    }

    let _ = rep(
      "(println (str \"Calisp [\" *host-language* \"]\"))",
      &self.repl_env,
    );
    loop {
      let readline = rl.readline("calisp interactive: ");
      match readline {
        Ok(line) => {
          rl.add_history_entry(&line);
          rl.save_history(".calisp-history").unwrap();
          if line.len() > 0 {
            match rep(&line, &self.repl_env) {
              Ok(out) => println!("{}", out),
              Err(e) => println!("Error: {}", format_error(e)),
            }
          }
        }
        Err(ReadlineError::Interrupted) => break,
        Err(ReadlineError::Eof) => break,
        Err(err) => {
          println!("Error: {:?}", err);
          break;
        }
      }
    }
  }
}
