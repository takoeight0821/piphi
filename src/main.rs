mod eval;
mod parser;
mod syntax;

use anyhow::Result;
use clap::Parser;
use parser::lexer::remove_whitespace;
use parser::lexer::tokenize;
use parser::parse;
use std::collections::HashMap;
use std::io::Read;
use std::path::PathBuf;
use std::rc::Rc;

use crate::eval::Evaluator;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Source file
    #[arg(value_name = "FILE")]
    source: Option<PathBuf>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let src: String = if let Some(source) = cli.source.as_deref() {
        // read src
        std::fs::read_to_string(source)?
    } else {
        // read stdin until EOF
        let mut str = String::new();
        std::io::stdin().lock().read_to_string(&mut str)?;
        str
    };

    let tokens = tokenize(&src)?;
    let ast = parse(remove_whitespace(&tokens))?;

    println!("{}", ast);

    let mut evaluator = Evaluator::new();
    let value = evaluator.eval(Rc::new(HashMap::new()), &ast);

    println!("{:?}", value);

    Ok(())
}
