#![allow(non_snake_case)]
#![allow(dead_code)]

use std::rc::Rc;

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

/// Read function for reading plain text and parsing it into AST
fn read(str: &str) -> CalispRet {
    reader::read_str(str.to_string())
}

/// Handle quasiquotes `\``
fn quasiquote(ast: &CalispVal) -> CalispVal {
    match ast {
        List(ref v, _) | Vector(ref v, _) if v.len() > 0 => {
            let a0 = &v[0];
            match a0 {
                Sym(ref s) if s == "unquote" => v[1].clone(),
                _ => match a0 {
                    List(ref v0, _) | Vector(ref v0, _) if v0.len() > 0 => match v0[0] {
                        Sym(ref s) if s == "splice-unquote" => list![
                            Sym("concat".to_string()),
                            v0[1].clone(),
                            quasiquote(&list!(v[1..].to_vec()))
                        ],
                        _ => list![
                            Sym("cons".to_string()),
                            quasiquote(a0),
                            quasiquote(&list!(v[1..].to_vec()))
                        ],
                    },
                    _ => list![
                        Sym("cons".to_string()),
                        quasiquote(a0),
                        quasiquote(&list!(v[1..].to_vec()))
                    ],
                },
            }
        }
        _ => list![Sym("quote".to_string()), ast.clone()],
    }
}

/// Check if call is macro call
fn is_macro_call(ast: &CalispVal, env: &Env) -> Option<(CalispVal, CalispArgs)> {
    match ast {
        List(v, _) => match v[0] {
            Sym(ref s) => match env_find(env, s) {
                Some(e) => match env_get(&e, &v[0]) {
                    Ok(f @ CalispFunc { is_macro: true, .. }) => Some((f, v[1..].to_vec())),
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
            for a in v.iter() {
                lst.push(eval(a.clone(), env.clone())?)
            }
            Ok(list!(lst))
        }
        Vector(v, _) => {
            let mut lst: CalispArgs = vec![];
            for a in v.iter() {
                lst.push(eval(a.clone(), env.clone())?)
            }
            Ok(vector!(lst))
        }
        Hash(hm, _) => {
            let mut new_hm: FnvHashMap<String, CalispVal> = FnvHashMap::default();
            for (k, v) in hm.iter() {
                new_hm.insert(k.to_string(), eval(v.clone(), env.clone())?);
            }
            Ok(Hash(Rc::new(new_hm), Rc::new(Nil)))
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
                if l.len() == 0 {
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

                if l.len() == 0 {
                    return Ok(ast);
                }
                let a0 = &l[0];
                match a0 {
                    Sym(ref a0sym) if a0sym == "def!" => {
                        env_set(&env, l[1].clone(), eval(l[2].clone(), env.clone())?)
                    }
                    Sym(ref a0sym) if a0sym == "let*" => {
                        env = env_new(Some(env.clone()));
                        let (a1, a2) = (l[1].clone(), l[2].clone());
                        match a1 {
                            List(ref binds, _) | Vector(ref binds, _) => {
                                for (b, e) in binds.iter().tuples() {
                                    match b {
                                        Sym(_) => {
                                            let _ = env_set(
                                                &env,
                                                b.clone(),
                                                eval(e.clone(), env.clone())?,
                                            );
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
                    Sym(ref a0sym) if a0sym == "quote" => Ok(l[1].clone()),
                    Sym(ref a0sym) if a0sym == "quasiquote" => {
                        ast = quasiquote(&l[1]);
                        continue 'tco;
                    }
                    Sym(ref a0sym) if a0sym == "defmacro!" => {
                        let (a1, a2) = (l[1].clone(), l[2].clone());
                        let r = eval(a2, env.clone())?;
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
                                    meta: Rc::new(Nil),
                                },
                            )?),
                            _ => error("set_macro on non-function"),
                        }
                    }
                    Sym(ref a0sym) if a0sym == "macroexpand" => {
                        match macroexpand(l[1].clone(), &env) {
                            (_, Ok(new_ast)) => Ok(new_ast),
                            (_, e) => return e,
                        }
                    }
                    Sym(ref a0sym) if a0sym == "try*" => match eval(l[1].clone(), env.clone()) {
                        Err(ref e) if l.len() >= 3 => {
                            let exc = match e {
                                ErrCalispVal(cv) => cv.clone(),
                                ErrString(s) => Str(s.to_string()),
                            };
                            match l[2].clone() {
                                List(c, _) => {
                                    let catch_env = env_bind(
                                        Some(env.clone()),
                                        list!(vec![c[1].clone()]),
                                        vec![exc],
                                    )?;
                                    eval(c[2].clone(), catch_env)
                                }
                                _ => error("invalid catch block"),
                            }
                        }
                        res => res,
                    },
                    Sym(ref a0sym) if a0sym == "do" => {
                        match eval_ast(&list!(l[1..l.len() - 1].to_vec()), &env)? {
                            List(_, _) => {
                                ast = l.last().unwrap_or(&Nil).clone();
                                continue 'tco;
                            }
                            _ => error("invalid do form"),
                        }
                    }
                    Sym(ref a0sym) if a0sym == "if" => {
                        let cond = eval(l[1].clone(), env.clone())?;
                        match cond {
                            Bool(false) | Nil if l.len() >= 4 => {
                                ast = l[3].clone();
                                continue 'tco;
                            }
                            Bool(false) | Nil => Ok(Nil),
                            _ if l.len() >= 3 => {
                                ast = l[2].clone();
                                continue 'tco;
                            }
                            _ => Ok(Nil),
                        }
                    }
                    Sym(ref a0sym) if a0sym == "fn*" => {
                        let (a1, a2) = (l[1].clone(), l[2].clone());
                        Ok(CalispFunc {
                            eval: eval,
                            ast: Rc::new(a2),
                            env: env,
                            params: Rc::new(a1),
                            is_macro: false,
                            meta: Rc::new(Nil),
                        })
                    }
                    Sym(ref a0sym) if a0sym == "eval" => {
                        ast = eval(l[1].clone(), env.clone())?;
                        while let Some(ref e) = env.clone().outer {
                            env = e.clone();
                        }
                        continue 'tco;
                    }
                    _ => match eval_ast(&ast, &env)? {
                        List(ref el, _) => {
                            let ref f = el[0].clone();
                            let args = el[1..].to_vec();
                            match f {
                                Func(_, _) => f.apply(args),
                                CalispFunc {
                                    ast: mast,
                                    env: menv,
                                    params,
                                    ..
                                } => {
                                    let a = &**mast;
                                    let p = &**params;
                                    env = env_bind(Some(menv.clone()), p.clone(), args)?;
                                    ast = a.clone();
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
pub struct CalispInterpreter {
    input_file: String,
    repl_env: Env,
}

impl CalispInterpreter {
    /// Create new instance of [`CalispInterpreter`]
    fn new(input_file: String, arguments: &Vec<std::string::String>) -> CalispInterpreter {
        CalispInterpreter {
            input_file: input_file,
            repl_env: CalispInterpreter::new_env(arguments.to_vec()),
        }
    }

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

    /// Read code from the stdi
    pub fn run_interactive(&self) {
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

extern crate clap;
use clap::{crate_authors, crate_name, crate_version, App, Arg};

/// This function is run in standalone mode
/// Use `calisp interactive <additional arguments>` for an interactive mode
fn main() {
  let matches = App::new(crate_name!())
    .version(crate_version!())
    .author(crate_authors!())
    .arg(
      Arg::with_name("input_file")
        .help("Specify input file to be read")
        .short("i")
        .long("input-file")
        .value_name("FILE")
        .takes_value(true),
    )
    .subcommand(App::new("interactive").about("Run calisp in interactive mode"))
    .get_matches();

  match matches.subcommand_name() {
    Some("interactive") => {
      CalispInterpreter::new(
        "".to_string(),
        &matches
          .subcommand_matches("interactive")
          .unwrap()
          .args
          .iter()
          .map(|(key, _)| key.to_string())
          .collect(),
      )
      .run_interactive();
    }
    _ => {
      if let Some(input_file) = matches.value_of("input_file") {
          CalispInterpreter::new(
          input_file.to_string(),
          &matches
            .args
            .iter()
            .map(|(key, _)| key.to_string())
            .collect(),
        )
              .run().unwrap();
      }
    }
  }
}
