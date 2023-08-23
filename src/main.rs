mod eval;
mod parser;
mod syntax;

use anyhow::Result;
use clap::Parser;
use eval::{eval, flatten};
use parser::{
    lexer::{remove_whitespace, tokenize},
    parse,
};
use std::{io::Read, path::PathBuf};

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

    let ast = flatten(ast);
    let value = eval(eval::new_env(), ast);

    println!("{:?}", value);

    Ok(())
}
