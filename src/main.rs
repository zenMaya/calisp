extern crate clap;
use clap::{crate_authors, crate_name, crate_version, App, Arg};

use calisp::CalispInterpreter;

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

      CalispInterpreter::new(
        "".to_string(),
        &vec![]
      )
        .run_interactive();

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
        .run()
        .unwrap();
      }
    }
  }
}

