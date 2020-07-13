use std::fs::File;
use std::io::Read;
use std::sync::{RwLock};
use std::time::{SystemTime, UNIX_EPOCH};

extern crate rustyline;
use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::printer::pr_seq;
use crate::reader::read_str;
use crate::types::CalispErr::ErrCalispVal;
use crate::types::CalispVal::{
    Atom, Bool, CalispFunc, Func, Hash, Int, List, Nil, Str, Sym, Vector,
};
use crate::types::{
    CalispArgs, CalispRet, CalispVal, _assoc, _dissoc, atom, error, func, hash_map,
};
use crate::interop::CalispInterop;

macro_rules! fn_t_int_int {
    ($ret:ident, $fn:expr) => {{
        |a: CalispArgs| match (a[0].clone(), a[1].clone()) {
            (Int(a0), Int(a1)) => Ok($ret($fn(a0, a1))),
            _ => error("expecting (int,int) args"),
        }
    }};
}

macro_rules! fn_is_type {
    ($($ps:pat),*) => {{
        |a:CalispArgs| { Ok(Bool(match a[0] { $($ps => true,)* _ => false})) }
    }};
    ($p:pat if $e:expr) => {{
        |a:CalispArgs| { Ok(Bool(match a[0] { $p if $e => true, _ => false})) }
    }};
    ($p:pat if $e:expr,$($ps:pat),*) => {{
        |a:CalispArgs| { Ok(Bool(match a[0] { $p if $e => true, $($ps => true,)* _ => false})) }
    }};
}

macro_rules! fn_str {
    ($fn:expr) => {{
        |a: CalispArgs| match a[0].clone() {
            Str(a0) => $fn(a0),
            _ => error("expecting (str) arg"),
        }
    }};
}

fn symbol(a: CalispArgs) -> CalispRet {
    match a[0] {
        Str(ref s) => Ok(Sym(s.to_string())),
        _ => error("illegal symbol call"),
    }
}

fn readline(a: CalispArgs) -> CalispRet {
    lazy_static! {
        static ref RL: RwLock<Editor<()>> = RwLock::new(Editor::<()>::new());
    }

    match a[0] {
        Str(ref p) => match RL.write().unwrap().readline(p) {
            Ok(mut line) => {
                if line.ends_with('\n') {
                    line.pop();
                    if line.ends_with('\r') {
                        line.pop();
                    }
                }
                Ok(Str(line))
            }
            Err(ReadlineError::Eof) => Ok(Nil),
            Err(e) => error(&format!("{:?}", e)),
        },
        _ => error("readline: prompt is not Str"),
    }
}

fn slurp(f: String) -> CalispRet {
    let mut s = String::new();
    match File::open(f).and_then(|mut f| f.read_to_string(&mut s)) {
        Ok(_) => Ok(Str(s)),
        Err(e) => error(&e.to_string()),
    }
}

fn time_ms(_a: CalispArgs) -> CalispRet {
    let ms_e = match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(d) => d,
        Err(e) => return error(&format!("{:?}", e)),
    };
    Ok(Int(
        ms_e.as_secs() as i64 * 1000 + ms_e.subsec_nanos() as i64 / 1_000_000
    ))
}

fn get(a: CalispArgs) -> CalispRet {
    match (a[0].clone(), a[1].clone()) {
        (Nil, _) => Ok(Nil),
        (Hash(ref hm, _), Str(ref s)) => match hm.read().unwrap().get(s) {
            Some(cv) => Ok(cv.clone()),
            None => Ok(Nil),
        },
        _ => error("illegal get args"),
    }
}

fn assoc(a: CalispArgs) -> CalispRet {
    match a[0] {
        Hash(ref hm, _) => _assoc((**hm).read().unwrap().clone(), a[1..].to_vec()),
        _ => error("assoc on non-Hash Map"),
    }
}

fn dissoc(a: CalispArgs) -> CalispRet {
    match a[0] {
        Hash(ref hm, _) => _dissoc((**hm).read().unwrap().clone(), a[1..].to_vec()),
        _ => error("dissoc on non-Hash Map"),
    }
}

fn contains_q(a: CalispArgs) -> CalispRet {
    match (a[0].clone(), a[1].clone()) {
        (Hash(ref hm, _), Str(ref s)) => Ok(Bool(hm.read().unwrap().contains_key(s))),
        _ => error("illegal get args"),
    }
}

fn keys(a: CalispArgs) -> CalispRet {
    match a[0] {
        Hash(ref hm, _) => Ok(list!(hm.read().unwrap().keys().map(|k| { Str(k.to_string()) }).collect())),
        _ => error("keys requires Hash Map"),
    }
}

fn vals(a: CalispArgs) -> CalispRet {
    match a[1] {
        Hash(ref hm, _) => Ok(list!(hm.read().unwrap().values().map(|v| { v.clone() }).collect())),
        _ => error("vals requires Hash Map"),
    }
}

fn cons(a: CalispArgs) -> CalispRet {
    match a[1].clone() {
        List(v, _) | Vector(v, _) => {
            let mut new_v = vec![a[0].clone()];
            new_v.extend_from_slice(&v.read().unwrap());
            Ok(list!(new_v.to_vec()))
        }
        _ => error("cons expects seq as second arg"),
    }
}

fn concat(a: CalispArgs) -> CalispRet {
    let mut new_v = vec![];
    for seq in a.iter() {
        match seq {
            List(v, _) | Vector(v, _) => new_v.extend_from_slice(&v.read().unwrap()),
            _ => return error("non-seq passed to concat"),
        }
    }
    Ok(list!(new_v.to_vec()))
}

fn nth(a: CalispArgs) -> CalispRet {
    match (a[0].clone(), a[1].clone()) {
        (List(seq, _), Int(idx)) | (Vector(seq, _), Int(idx)) => {
            if seq.read().unwrap().len() <= idx as usize {
                return error("nth: index out of range");
            }
            Ok(seq.read().unwrap()[idx as usize].clone())
        }
        _ => error("invalid args to nth"),
    }
}

fn first(a: CalispArgs) -> CalispRet {
    match a[0].clone() {
        List(ref seq, _) | Vector(ref seq, _) if seq.read().unwrap().len() == 0 => Ok(Nil),
        List(ref seq, _) | Vector(ref seq, _) => Ok(seq.read().unwrap()[0].clone()),
        Nil => Ok(Nil),
        _ => error("invalid args to car (first)"),
    }
}

fn rest(a: CalispArgs) -> CalispRet {
    match a[0].clone() {
        List(ref seq, _) | Vector(ref seq, _) => {
            if seq.read().unwrap().len() > 1 {
                Ok(list!(seq.read().unwrap()[1..].to_vec()))
            } else {
                Ok(list![])
            }
        }
        Nil => Ok(list![]),
        _ => error("invalid args to cdr (rest)"),
    }
}

fn apply(a: CalispArgs) -> CalispRet {
    match a[a.len() - 1] {
        List(ref v, _) | Vector(ref v, _) => {
            let f = &a[0];
            let mut fargs = a[1..a.len() - 1].to_vec();
            fargs.extend_from_slice(&v.read().unwrap());
            f.apply(fargs)
        }
        _ => error("apply called with non-seq"),
    }
}

fn map(a: CalispArgs) -> CalispRet {
    match a[1] {
        List(ref v, _) | Vector(ref v, _) => {
            let mut res = vec![];
            for cv in v.read().unwrap().iter() {
                res.push(a[0].apply(vec![cv.clone()])?)
            }
            Ok(list!(res))
        }
        _ => error("map called with non-seq"),
    }
}

fn conj(a: CalispArgs) -> CalispRet {
    match &a[0] {
        List(v, _) => {
            let sl = a[1..]
                .iter()
                .rev()
                .map(|a| a.clone())
                .collect::<Vec<CalispVal>>();
            Ok(list!([&sl[..], &v.read().unwrap()].concat()))
        }
        Vector(v, _) => Ok(vector!([&v.read().unwrap(), &a[1..]].concat())),
        _ => error("conj called with non-seq"),
    }
}

fn seq(a: CalispArgs) -> CalispRet {
    match &a[0] {
        List(v, _) | Vector(v, _) if v.read().unwrap().len() == 0 => Ok(Nil),
        List(v, _) | Vector(v, _) => Ok(list!(v.read().unwrap().to_vec())),
        Str(ref s) if s.len() == 0 => Ok(Nil),
        Str(ref s) if !a[0].keyword_q() => {
            Ok(list!(s.chars().map(|c| { Str(c.to_string()) }).collect()))
        }
        Nil => Ok(Nil),
        _ => error("seq called with non-seq"),
    }
}

fn exit(a: CalispArgs) -> CalispRet {
    if a.len() == 0 {
        std::process::exit(0)
    } else {
        match a[0] {
            Int(code) => std::process::exit(code as i32),
            _ => error("exit not valid exit code"),
        }
    }
}

fn describe_fn(a: CalispArgs) -> CalispRet {
    match &a[0] {
        Func(_, _, docstring) => Ok(Str(docstring.to_string())),
        CalispFunc{docstring, ..} => Ok(Str(docstring.to_string())),
        _ => error("not a function")
    }
}

fn help(a: CalispArgs) -> CalispRet {
    match &a[0] {
        Func(_, _, _) | CalispFunc{..} => {
            println!("{}", pr_seq(&vec!(describe_fn(a)?), false, "", "", " "));
            Ok(Nil)
        },
        _ => error("not implemented yet")
    }
}

pub fn ns() -> Vec<(&'static str, CalispVal)> {
    vec![
        ("=", func(|a| Ok(Bool(a[0] == a[1])), "(= VALUE VALUE) -> TRUE/FALSE
evals to true whenever 2 arguments are equal".to_string())),
        ("throw", func(|a| Err(ErrCalispVal(a[0].clone())), "(throw STRING) -> ERROR
throw an error 
use catch to catch the error".to_string())),
        ("nil?", func(fn_is_type!(Nil), "(nil? VALUE) -> TRUE/FALSE
takes a single argument and evals to true when value is nil".to_string())),
        ("false?", func(fn_is_type!(Bool(false)), "(false? VALUE) -> TRUE/FALSE
takes a single argument and evals to true when value is false".to_string())),
        ("nil?", func(fn_is_type!(Bool(true)), "(nil? VALUE) -> TRUE/FALSE
takes a single argument and evals to true when value is nil".to_string())),
        ("symbol", func(symbol, "(symbol SYMBOL) -> VALUE/ERROR
lookup the symbol in the environment structure and return the value or raise an error if no value is found".to_string())),
        ("nil?", func(fn_is_type!(Sym(_)), "(nil? VALUE) -> TRUE/FALSE
takes a single argument and evals to true when argument is nil".to_string())),
        (
            "string?",
            func(fn_is_type!(Str(ref s) if !s.starts_with("\u{29e}")), "(string? VALUE) -> TRUE/FALSE
evals to true when value is string".to_string()),
        ),
        ("keyword", func(|a| a[0].keyword(), "(keyword VALUE) -> KEYWORD
evals as a keyword(s) with the same name (usually just be prepending the special keyword unicode symbol). This function should also detect if the argument is already a keyword and just return it".to_string())),
        (
            "keyword?",
            func(fn_is_type!(Str(ref s) if s.starts_with("\u{29e}")), "(keyword? VALUE) -> TRUE/FALSE
takes a single argument and evals true when argument is keyword".to_string()),
        ),
        ("number?", func(fn_is_type!(Int(_)), "(number? VALUE) -> TRUE/FALSE
takes a single argument and evals true when value is number".to_string())),
        (
            "fn?",
            func(fn_is_type!(CalispFunc{is_macro,..} if !is_macro,Func(_,_,_)), "(fn? VALUE) -> TRUE/FALSE
takes a single argument and evals true when value is function".to_string()),
        ),
        (
            "macro?",
            func(fn_is_type!(CalispFunc{is_macro,..} if is_macro), "(macro? VALUE) -> TRUE/FALSE
takes a single argument and evals true when value is macro".to_string()),
        ),
        ("pr-str", func(|a| Ok(Str(pr_seq(&a, true, "", "", " "))),
                        "(pr-str VALUE) -> STRING
takes as a argument a data structure and return a string representation of it. But pr_str is much simpler and is basically just a switch statement on the type of the input object:

symbol: return the string name of the symbol
number: return the number as a string
list: iterate through each element of the list calling pr_str on it, then join the results with a space separator, and surround the final result with parens".to_string())),
        ("str", func(|a| Ok(Str(pr_seq(&a, false, "", "", ""))), "(str VALUE) -> NIL
prints every argument raw".to_string())),
        (
            "prn",
            func(|a| {
                println!("{}", pr_seq(&a, true, "", "", " "));
                Ok(Nil)
            }, "(prn VALUE) -> NIL
prints every argument on a new line formatted (for unformatted use println)".to_string()),
        ),
        (
            "println",
            func(|a| {
                println!("{}", pr_seq(&a, false, "", "", " "));
                Ok(Nil)
            }, "(println VALUE) -> NIL
prints every argument on a new line unformatted (for formatted use prn)".to_string()),
        ),
        ("read-str", func(fn_str!(|s| { read_str(s) }), "(read-str STRING) -> (eval)
reads calisp expression
tokenizes it (tokenize)
and then reads from it".to_string())),
        ("readline", func(readline, "(readline) -> STRING
read line from stdin".to_string())),
        ("slurp", func(fn_str!(|f| { slurp(f) }), "(slurp STRING) -> STRING
this function takes a file name (string) and returns the contents of the file as a string".to_string())),
        ("<", func(fn_t_int_int!(Bool, |i, j| { i < j }), "(< VALUE VALUE) -> TRUE/FALSE
takes two number arguments and evals true whenever first is smaller than second".to_string())),
        ("<=", func(fn_t_int_int!(Bool, |i, j| { i <= j }), "(<= VALUE VALUE) -> TRUE/FALSE
takes two number arguments and evals true whenever first is smaller or equal to second".to_string())),
        (">", func(fn_t_int_int!(Bool, |i, j| { i > j }), "(> VALUE VALUE) -> TRUE/FALSE
takes two number arguments and evals true whenever first is larger than second".to_string())),
        (">=", func(fn_t_int_int!(Bool, |i, j| { i >= j }), "(>= VALUE VALUE) -> TRUE/FALSE
takes two number arguments and evals true whenever first is larger or equal to second".to_string())),
        ("+", func(fn_t_int_int!(Int, |i, j| { i + j }), "(+ NUMBER NUMBER) -> NUMBER
takes two number arguments and evals sum of the two numbers".to_string())),
        ("-", func(fn_t_int_int!(Int, |i, j| { i - j }), "(- NUMBER NUMBER) -> NUMBER
takes two number arguments and evals second substracted from the first".to_string())),
        ("*", func(fn_t_int_int!(Int, |i, j| { i * j }), "(* NUMBER NUMBER) -> NUMBER
takes two number arguments and evals multiplication of the two".to_string())),
        ("/", func(fn_t_int_int!(Int, |i, j| { i / j }), "(/ NUMBER NUMBER) -> NUMBER
takes two number arguments and evals second divided by the first".to_string())),
        ("time-ms", func(time_ms, "(time-ms) -> NUMBER
evals as time from the unix epoch in miliseconds".to_string())),
        ("sequential?", func(fn_is_type!(List(_, _), Vector(_, _)), "(sequential? LIST/VECTOR) -> TRUE/FALSE
takes one argument and evals true when argument is a sequence".to_string())),
        ("list", func(|a| Ok(list!(a)), "(list VALUE..) -> LIST
evals to all arguments as a list".to_string())),
        ("list?", func(fn_is_type!(List(_, _)), "(list? VALUE) -> TRUE/FALSE
takes one argument and evals true when value is list".to_string())),
        ("vector", func(|a| Ok(vector!(a)), "(vector VALUE..) -> VECTOR
evals to all arguments as a list".to_string())),
        ("vector?", func(fn_is_type!(Vector(_, _)), "(vector? VALUE) -> TRUE/FALSE
takes one argument and evals true when value is list".to_string())),
        ("hash-map", func(|a| hash_map(a), "(hash-map (KEY VALUE)..) -> HASH-MAP
evals all arguments as hash maps".to_string())),
        ("hash-map?", func(fn_is_type!(Hash(_, _)), "(hash-map? HASH-MAP) -> TRUE/FALSE
takes one argument and evals true when value is hash-map".to_string())),
        ("assoc", func(assoc, "(assoc HASH-MAP (KEY VALUE)..) -> HASH-MAP
takes a hash-map as the first argument and the remaining arguments are odd/even key/value pairs to 'associate' (merge) into the hash-map. Note that the original hash-map is unchanged and the function eval is the new hash map".to_string())),
        ("dissoc", func(dissoc, "(dissoc HASH-MAP KEYS) -> HASH-MAP
takes a hash-map and a list of keys to remove from the hash-map. Note that the original hash-map is unchanged and a the eval is the new hash-map with the keys removed. Key arguments that do not exist in the hash-map are ignored".to_string())),
        ("get", func(get, "(get HASH-MAP KEY) -> VALUE
takes a hash-map and a key and evals as the value of looking up that key in the hash-map
if the key is not found then nil is returned".to_string())),
        ("contains?", func(contains_q, "(contains HASH-MAP) -> TRUE/NIL
takes a hash-map and a key and evals true when the key is found in the hash-map then evals nil".to_string())),
        ("keys", func(keys, "(keys HASH-MAP) -> KEYS
takes a hash-map and evals list of all keys in the hash-map".to_string())),
        ("vals", func(vals, "(vals HASH-MAP) -> VALUES
takes a hash-map and evals list of all values in the hash-map".to_string())),
        ("cons", func(cons, "(cons SEQUENCE SEQUENCE) -> LIST
takes two list arguments and evals as new list with second list prepended to first".to_string())),
        ("concat", func(concat, "(concat SEQUENCE..) -> LIST
takes 0 or more lists as arguments and evals new list that is concatenation (join) of all the lists".to_string())),
        ("empty?", func(|a| a[0].empty_q(), "(empty LIST..) -> TRUE/FALSE
takes the first argument and evals true when list is empty and false if it's not".to_string())),
        ("nth", func(nth, "(nth SEQUENCE INDEX) -> VALUE/ERROR
takes a list and a number and evals element at a given index
if the index is out of bounds, raises an exception".to_string())),
        ("first", func(first, "(first SEQUENCE) -> VALUE
evals as the first element of a sequence".to_string())),
        ("car", func(first, "(car SEQUENCE) -> VALUE
evals as the first element of a sequence".to_string())),
        ("rest", func(rest, "(rest SEQUENCE) -> LIST
evals as a list with all elements except for the first one".to_string())),
        ("cdr", func(rest, "(cdr SEQUENCE) -> LIST
evals as a list with all elements except for the first one".to_string())),
        ("count", func(|a| a[0].count(), "(count SEQUENCE) -> NUMBER
take a single argument, a sequence, and eval as count of all the elements".to_string())),
        ("apply", func(apply, "(apply FUNCTION &optional.. SEQUENCE) -> VALUE
takes at least two arguments. The first argument is a function and the last argument is list (or vector). The arguments between the function and the last argument (if there are any) are concatenated with the final argument to create the arguments that are used to call the function. The apply function allows a function to be called with arguments that are contained in a list (or vector). In other words, (apply F A B [C D]) is equivalent to (F A B C D)".to_string())),
        ("map", func(map, "(map FUNCTION SEQUENCE) -> LIST
takes a function and a list (or vector) and evaluates the function against every element of the list (or vector) one at a time and evals as list of results".to_string())),
        ("conj", func(conj, "(conj SEQUENCE SEQUENCE) -> SEQUENCE
takes a sequence and one or more elements as arguments and returns a new collection which includes the original collection and the new elements. If the collection is a list, a new list is returned with the elements inserted at the start of the given list in opposite order; if the collection is a vector, a new vector is returned with the elements added to the end of the given vector.".to_string())),
        ("seq", func(seq, "(seq SEQUENCE) -> LIST/NIL
takes a list, vector, string, or nil. If an empty list, empty vector, or empty string (\"\") is passed in then nil is returned. a list is evaled as a list
a vector is converted into a list
a string is converted to a list that containing the original string split into single character strings".to_string())),
        ("meta", func(|a| a[0].get_meta(), "(meta FUNCTION/SEQUENCE/HASH-MAP) -> VALUE
takes a single argument and evals as a value of the metadata atribute".to_string())),
        ("with-meta", func(|a| a[0].clone().with_meta(&a[1]), "(with-meta FUNCTION/SEQUENCE/HASH-MAP VALUE) -> FUNCTION/SEQUENCE/HASH-MAP
take two arguments and evals as the first value with second set as it's metadata value".to_string())),
        ("atom", func(|a| Ok(atom(&a[0])), "(atom VALUE) -> ATOM
takes a single value and evals it as a new atom pointing to tha value".to_string())),
        ("atom?", func(fn_is_type!(Atom(_)), "(atom VALUE) -> TRUE/FALSE
takes a single value and evals true if the value is atom".to_string())),
        ("deref", func(|a| a[0].deref(), "(deref ATOM) -> VALUE
takes an atom argument and evals as the value referenced by this atom.".to_string())),
        ("reset!", func(|a| a[0].reset_bang(&a[1]), "(reset ATOM VALUE) -> VALUE
takes an atom and value arguments
the atom is modified to point at that value
evals as the value".to_string())),
        ("swap!", func(|a| a[0].swap_bang(&a[1..].to_vec()), "(swap! ATOM FUNCTION &optional VALUE..) -> ATOM
takes an atom, a function, and zero or more function arguments
the atom's value is modified to the result of applying the function with the atom's value as the first argument and the optionally given function arguments as the rest of the arguments
evals as the new atom's value ".to_string())),
        ("exit", func(exit, "exit (exit &optional NUMBER) ->
takes a number as an optional argument
stops the execution of the program
uses the number as standard exit code".to_string())),
        ("rust-fn-eval", func(CalispInterop::eval_fn, "(rust-fn-eval STRING) -> VALUE/ERROR
takes a single string argument of name of the rust function
note that all rust functions must be registered in the Calisp::Interop::FUNCTIONS and are global
evals as the return value of that function
".to_string())),
        ("rust-var-set", func(CalispInterop::set_var, "(rust-var-set STRING VALUE) -> NIL/ERROR
takes two arguments name of the variable and it's new value
note that all rust variables must be registered in the Calisp::Interop::VARIABLES and are global
evals as nil when successful or throws an error
".to_string())),
        ("rust-var-get", func(CalispInterop::get_var, "(rust-var-get STRING) -> VALUE/ERROR
takes a single string argument of a name of the rust variable
note that all rust variables must be registered in the Calisp::Interop::VARIABLES and are global
evals as the value of that variable or throws an error
".to_string())),
        ("describe-fn", func(describe_fn, "(describe-fn FUNCTION) -> STRING
takes a single argument which is a function
evals as it's docstring".to_string())),
        ("help", func(help, "(help VALUE) -> NIL
takes a single argument which is anything within calisp
evals as nil and prints result in stdout".to_string()))
    ]
}
